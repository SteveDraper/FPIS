package exercises.monoid

import Monoid._

/**
 * Created by steve on 9/21/2015.
 */
sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC) = (a1, a2) match {
      case (Stub(lc), Stub(rc)) => Stub(lc + rc)
      case (Stub(lc), Part(plc,n,rc)) => Part(lc + plc, n, rc)
      case (Part(lc,n,prc),Stub(rc)) => Part(lc,n, prc + rc)
      case (Part(lc, n1, lmc),Part(rmc,n2,rc)) => Part(lc, n1+n2 + (if(!lmc.isEmpty || !rmc.isEmpty) 1 else 0),rc)
    }

    def zero = Stub("")
  }

  def wordsToWC(words: String): WC = words.length match {
    case 0 => wcMonoid.zero
    case 1 => if (words.charAt(0) == ' ') Part("",0,"") else Stub(words)
    case _ => {
      val (s1,s2) = words splitAt(words.length/2)
      wcMonoid.op(wordsToWC(s1),wordsToWC(s2))
    }
  }

  def wordsToWC2(words: String): WC = foldMap(words.toIndexedSeq, wcMonoid)(c => if (c.isWhitespace) Part("",0,"") else Stub(c.toString))
  def countWords(words: String): Int = wordsToWC2(words) match {
    case Stub(_) => 1
    case Part("",n,"") => n
    case Part(l,n,"") if (!l.isEmpty) => n+1
    case Part("",n,r) if (!r.isEmpty) => n+1
    case Part(_,n,_) => n+2
  }
}
