package exercises.parsers

import scala.util.matching.Regex

/**
 * Created by steve on 9/14/2015.
 */
trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  def char(c: Char): Parser[Char] = string(c.toString) map(_.charAt(0))
  def anyChar: Parser[Char]

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]
  def map2[A,B,C](p1: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = flatMap(p1)(a=>map(p2)(b => f(a,b)))
  def andThen[A,B](p1: Parser[A], p2: => Parser[B]): Parser[(A,B)] = map2(p1,p2)((_,_))
  def slice[A](p: Parser[A]): Parser[String]
  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def zero[A](a: A): Parser[A]
  def fail[A](reason: String): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    val pList = List.fill(n)(p)

    pList.foldRight(zero(Nil:List[A]))((ps,paa)=>map2(ps,paa)((a,sa)=>a::sa))
  }

  def map[A,B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p)(a=>zero(f(a)))
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def many[A](p: Parser[A]): Parser[List[A]] = {
    ((p andThen many(p)) map (p=>p._1::p._2)) | zero(Nil:List[A])
  }

  def many1[A](p: Parser[A]): Parser[List[A]] = (p andThen many(p)) map (p=>p._1::p._2)

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def andThen[B](p2: => Parser[B]): Parser[(A,B)] = self.andThen(p,p2)
    def slice: Parser[String] = self.slice(p)
    def many[A] = self.many(p)
  }
}

case class MyParseError(error: String)

case class MyParser[+A](f: String => (Either[MyParseError,A], String)) {
  def run(input: String): Either[MyParseError,A] = f(input) match {
    case (Left(f),_) => Left(f)
    case (x,"") => x
    case (_,s) => Left(MyParseError(s"Unconsumed tail: $s"))
  }

  def internalRun(input: String): (Either[MyParseError,A],String) = f(input)
}

object MyParsers extends Parsers[MyParseError,MyParser] {
  def run[A](p: MyParser[A])(input: String): Either[MyParseError,A] = p.run(input)

  def unit[A](a: A): MyParser[A] = MyParser[A](input=>
    if (input.startsWith(a.toString)) (Right(a),input.substring(a.toString.length)) else (Left(MyParseError(s"Mismatched: $a")),input)
  )
  def zero[A](a: A): MyParser[A] = MyParser[A](input => (Right(a),input))
  def anyChar: MyParser[Char] = MyParser[Char](input=>
    if (input.isEmpty) (Left(MyParseError("Empty string for any match")),"") else (Right(input.charAt(0)),input.substring(1)))

  def fail[A](reason: String) = MyParser[A](input => (Left(MyParseError(reason)), input))

  def slice[A](p: MyParser[A]): MyParser[String] = MyParser[String]{
    input => {
      p.internalRun(input) match {
        case (Right(a),remaining) => (Right(input.substring(0, input.length - remaining.length)),remaining)
        case (Left(f), _) => (Left(f),input)
      }
    }
  }

  def flatMap[A,B](p: MyParser[A])(f: A => MyParser[B]): MyParser[B] = MyParser[B]{
    input=> {
      p.internalRun(input) match {
        case (Left(f),_) => (Left(f),input)
        case (Right(r), remaining) => f(r).internalRun(input.substring(input.length-remaining.length))
      }
    }
  }

  def or[A](p1: MyParser[A], p2: MyParser[A]): MyParser[A] = MyParser[A](
    input => {
      p1.internalRun(input) match {
        case (Left(e),_) => p2.internalRun(input)
        case x => x
      }
    }
  )
  implicit def string(s: String) = unit(s)
  implicit def regex(r: Regex): MyParser[String] = MyParser[String]{
    input => {
      r findPrefixOf(input) match {
        case None => (Left(MyParseError(s"Regex ${r.toString} failed to match $input")), input)
        case Some(m) => (Right(m),input.substring(m.length))
      }
    }
  }
}


