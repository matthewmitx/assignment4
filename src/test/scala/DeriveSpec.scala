package edu.ucsb.cs.cs162.regex.derivative

import org.scalatest._
import edu.ucsb.cs.cs162.regex._

class DeriveSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  val b = Chars('b')
  val c = Chars('c')
  val d = Chars('d')
  val e = EmptyString
  val f = ∅

  val r = b ~ c
  val r1 = (b | (c ~ d)).*
  val r2 = ((b ~ d) | ((b.*) & (b | c)))
  val r3 = (r.^(3) | d.>=(2))
  val r4 = b.* ~ d.?
  val r5 = !(c.<>(4,6))
  val r6 = ((b ~ c ).* & (b ~ c ~ b ~ c))

  val l1 = new DerivativeMachine(r1)
  val l2 = new DerivativeMachine(r2)
  val l3 = new DerivativeMachine(r3)
  val l4 = new DerivativeMachine(r4)
  val l5 = new DerivativeMachine(r5)
  val l6 = new DerivativeMachine(r6)

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "matches"
    
  it should "recognize strings in the language 1" in { 
    (Derive.matches(r1, "bcdbbbbb")) should equal (true)
    (Derive.matches(r1, "bbcdbbcdbcd")) should equal (true)
  }

  it should "not recognize strings not in the language 1" in { 
    (Derive.matches(r1, "d")) should equal (false)
    (Derive.matches(r1, "cdc")) should equal (false)
  }

  it should "recognize strings in the language 2" in { 
    (Derive.matches(r2, "b")) should equal (true)
    (Derive.matches(r2, "bd")) should equal (true)
  }

  it should "not recognize strings not in the language 2" in { 
    (Derive.matches(r2, "bbbb")) should equal (false)
    (Derive.matches(r2, "c")) should equal (false)
  }

  it should "recognize strings in the language 3" in { 
    (Derive.matches(r3, "bcbcbc")) should equal (true)
    (Derive.matches(r3, "ddd")) should equal (true)
  }

  it should "not recognize strings not in the language 3" in { 
    (Derive.matches(r3, "bcb")) should equal (false)
    (Derive.matches(r3, "d")) should equal (false)
  }

  it should "recognize strings in the language 4" in { 
    (Derive.matches(r4, "d")) should equal (true)
    (Derive.matches(r4, "")) should equal (true)
  }

  it should "not recognize strings not in the language 4" in { 
    (Derive.matches(r4, "bbdd")) should equal (false)
    (Derive.matches(r4, "cd")) should equal (false)
  }

  it should "recognize strings in the language 5" in { 
    (Derive.matches(r5, "c")) should equal (true)
    (Derive.matches(r5, "ccccccc")) should equal (true)
  }

  it should "not recognize strings not in the language 5" in { 
    (Derive.matches(r5, "cccc")) should equal (false)
    (Derive.matches(r5, "cccccc")) should equal (false)
  }

  behavior of "eval"

  it should "recognize strings in the language 1" in { 
    (l1.eval("bcdbbbbb")) should equal (true)
    (l1.eval("bbcdbbcdbcd")) should equal (true)
  }

  it should "not recognize strings not in the language 1" in { 
    (l1.eval("d")) should equal (false)
    (l1.eval("cdc")) should equal (false)
  }

  it should "recognize strings in the language 2" in { 
    (l2.eval("b")) should equal (true)
    (l2.eval("bd")) should equal (true)
  }

  it should "not recognize strings not in the language 2" in { 
    (l2.eval("bbbb")) should equal (false)
    (l2.eval("c")) should equal (false)
  }

  it should "recognize strings in the language 3" in { 
    (l3.eval("bcbcbc")) should equal (true)
    (l3.eval("ddd")) should equal (true)
  }

  it should "not recognize strings not in the language 3" in { 
    (l3.eval("bcb")) should equal (false)
    (l3.eval("d")) should equal (false)
  }

  it should "recognize strings in the language 4" in { 
    (l4.eval("d")) should equal (true)
    (l4.eval("")) should equal (true)
  }

  it should "not recognize strings not in the language 4" in { 
    (l4.eval("bbdd")) should equal (false)
    (l4.eval("cd")) should equal (false)
  }

  it should "recognize strings in the language 5" in { 
    (l5.eval("c")) should equal (true)
    (l5.eval("ccccccc")) should equal (true)
  }

  it should "not recognize strings not in the language 5" in { 
    (l5.eval("cccc")) should equal (false)
    (l5.eval("cccccc")) should equal (false)
  }

  behavior of "derive"

  it should "take the derivative of language 1 with respect to a character" in {
    (l1.derive('b')) should equal (((b | (c ~ d))).*)
    (l1.derive('c')) should equal (d ~ ((b | (c ~ d))).*)
    (l1.derive('d')) should equal (∅)
  }

  it should "take the derivative of language 2 with respect to a character" in {
    (l2.derive('b')) should equal (d | ((b).* & ε))
    (l2.derive('c')) should equal (∅)
    (l2.derive('d')) should equal (∅)
  }

  it should "take the derivative of language 3 with respect to a character" in {
    (l3.derive('b')) should equal ((c ~ (b ~ c)) ~ (b ~ c))
    (l3.derive('c')) should equal (∅)
    (l3.derive('d')) should equal (d ~ (d).*)
  }

  it should "take the derivative of language 4 with respect to a character" in {
    (l4.derive('b')) should equal ((b).* ~ (ε | d))
    (l4.derive('c')) should equal (∅)
    (l4.derive('d')) should equal (ε)
  }

  it should "take the derivative of language 5 with respect to a character" in {
    (l5.derive('b')) should equal ((α).*)
    (l5.derive('c')) should equal (!(((((c ~ c) ~ c) ~ (c).*) & (((((ε | c) | (c ~ c)) | ((c ~ c) ~ c)) | (((c ~ c) ~ c) ~ c)) | ((((c ~ c) ~ c) ~ c) ~ c)))))
    (l5.derive('d')) should equal ((α).*)
  }

  it should "take the derivative of language 6 with respect to a character" in {
    (l6.derive('b')) should equal ((c ~ ((b ~ c)).*) & ((c ~ b) ~ c))
    (l6.derive('c')) should equal (∅)
    (l6.derive('d')) should equal (∅)
  }
}