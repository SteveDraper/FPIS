import exercises.parsers._
import MyParsers._
import JSON._
string("abra") run "abra"
val p1: MyParser[String] = "abra"
val p2: MyParser[String] = "cadabra"
(p1 | p2).run("cadabra")
val both = map2(p1,p2)(_+_)
both.run("abracadabra")
val abra3 = listOfN(3,p1)
abra3.run("abraabraabra")
val m = many(p1)
m.run("abraabraabra")
val a_len = many(char('a')).slice map (_.length)
val leading_a_len = (a_len andThen many(anyChar)) map (x=>x._1)
a_len.run("aaaa")
leading_a_len.run("aaaaab")
def numCharsToInt(cs: List[Char]): Int = (cs mkString).toInt
val numericChar = anyChar flatMap { c => if (c >= '0' && c <= '9') zero(c) else fail(s"Numeric character expected, got $c")}
val number = many1(numericChar) map numCharsToInt
val number2 = regex("[0-9]+".r) map(_.toInt)
val countedAs = number2 flatMap { n => listOfN(n,char('a'))}
countedAs.run("2aa")
number2.run("2")

val JS = JSONParser(MyParsers)

JS.run(" {}")

val spaces = regex("\\s?".r).slice

def trimLeft[A](p: MyParser[A]): MyParser[A] = (spaces andThen p) map (x => x._2)

trimLeft(char('{')).run("{")