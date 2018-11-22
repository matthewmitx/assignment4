package edu.ucsb.cs.cs162.regex.derivative

import org.scalatest._
import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.util._
import edu.ucsb.cs.cs162.range_set._
import edu.ucsb.cs.cs162.dfa._
import org.scalatest.Matchers._

class DerivativeAnalysisSpec extends FlatSpec with Matchers with Timeout {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  // The timeout in milliseconds for potentially slow code.
  val timeout = 2000
  val charB = Chars('b')
  val charC = Chars('c')
  val charD = Chars('d')
  val charE = Chars('e')

  // Analyze the given expression subject to a timeout.
  def analyzeWithTimeout(re: Regex) =
    timeoutAfter(timeout) { DerivativeAnalysis.analyze(re) }

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "the analysis"

  it should "should always terminate 1" in {
    val charA = Chars('a')

    // Causes a timeout or stack overflow if expression similarity isn't
    // implemented correctly.
    val dfa = analyzeWithTimeout((charA | (charA ~ charA)).*)
  }

  it should "should always terminate 2" in {
    // This test should test that check if normalization and DFA
    // building work well together. If the regexes are not conflated
    // properly, DFA construction would cause a timeout or stack
    // overflow and this test should fail.
    val charA = Chars('a')
    val charB = Chars('b')

    val dfa = analyzeWithTimeout(((charB | charA).* | (charA ~ charB).*).*)
  }

  it should "should always terminate 3" in {
    val charA = Chars('a')
    val charB = Chars('b')
    val charC = Chars('c')

    val dfa = analyzeWithTimeout(((charA ~ charB ~ charC).* | (charA.* | charB.*) | charC.*).*)
  }

  it should "should always terminate 4" in {
    val charA = Chars('a')
    val charB = Chars('b')
    val charC = Chars('c')

    val dfa = analyzeWithTimeout((charB.* | charC).* ~ charA.* | charB.* | !(charA.*) | (!charB).*)
  }

  it should "produce a DFA that recognizes the strings in language 1" in {
    val charA = Chars('a')

    val dfa = analyzeWithTimeout(ε | charA)

    dfa.matches("") should equal (true)
    dfa.matches("a") should equal (true)
  }

  it should "produce a DFA that recognizes the strings in language 2" in {
    val dfa = analyzeWithTimeout(((charB ~ charC) ~ charD) | charE)

    dfa.matches("bcd") should equal (true)
    dfa.matches("e") should equal (true)
  }

  it should "produce a DFA that recognizes the strings in language 3" in {
    val dfa = analyzeWithTimeout(charB & charB.* | charC.*)

    dfa.matches("b") should equal (true)
    dfa.matches("ccccccc") should equal (true)
  }

  it should "produce a DFA that recognizes the strings in language 4" in {
    val dfa = analyzeWithTimeout(!charB ~ (charD.* | (charE ~ charC)))

    dfa.matches("ddd") should equal (true)
    dfa.matches("ec") should equal (true)
  }

  it should "produce a DFA that recognizes the strings in language 5" in {
    val dfa = analyzeWithTimeout((charB | charC) ~ (charC | charD) ~ (charD | charE.*) ~ (charB.* | charB))

    dfa.matches("cceeeeeebbbbbb") should equal (true)
    dfa.matches("bcdb") should equal (true)
  }

  it should "produce a DFA that should not recognize strings not in the language 1" in {
    val charA = Chars('a')

    val dfa = analyzeWithTimeout(ε | charA)

    dfa.matches("b") should equal (false)
    dfa.matches("aa") should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 2" in {
    val dfa = analyzeWithTimeout(((charB ~ charC) ~ charD) | charE)

    dfa.matches("bcde") should equal (false)
    dfa.matches("ee") should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 3" in {
    val dfa = analyzeWithTimeout(charB & charB.* | charC.*)

    dfa.matches("bbbb") should equal (false)
    dfa.matches("d") should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 4" in {
    val dfa = analyzeWithTimeout(!charB ~ (charD.* | (charE ~ charC)) & !charC)

    dfa.matches("c") should equal (false)
    dfa.matches("b") should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 5" in {
    val dfa = analyzeWithTimeout((charB | charC) ~ (charC | charD) ~ (charD | charE.*) ~ (charB.* | charB))

    dfa.matches("bced") should equal (false)
    dfa.matches("cceeeebe") should equal (false)
  }

  it should "produce a DFA that has the correct structure 1" in {
    val a = Chars('a')
    val b = Chars('b')
    val Σ = CharSet(Char.MinValue → Char.MaxValue)
    val aSet = CharSet('a')
    val bSet = CharSet('b')

    val dfa = DerivativeAnalysis.analyze(a ~ b)

    dfa.init shouldEqual (a ~ b)
    dfa.fin shouldEqual(Set[Regex](ε))

    val trans : Map[Regex, Seq[(CharSet, Regex)]] = dfa.delta
    trans.keySet should contain theSameElementsAs List(a ~ b, b, ε, ∅)

    dfa.delta(a ~ b) should contain theSameElementsAs Seq((!aSet, Regex.∅), (aSet, b))
    dfa.delta(b) should contain theSameElementsAs Seq((!bSet, ∅), (bSet, ε))
    dfa.delta(ε) should contain theSameElementsAs Seq((Σ, ∅))
    dfa.delta(∅) should contain theSameElementsAs Seq((Σ, ∅))
  }

  it should "produce a DFA that has the correct structure 2" in {
    val a = Chars('a')
    val b = Chars('b')
    val Σ = CharSet(Char.MinValue → Char.MaxValue)
    val abSet = CharSet('a' -> 'b')

    val dfa = DerivativeAnalysis.analyze(a | b)

    dfa.init shouldEqual (a | b)
    dfa.fin shouldEqual(Set[Regex](ε))

    val trans : Map[Regex, Seq[(CharSet, Regex)]] = dfa.delta
    trans.keySet should contain theSameElementsAs List((a | b), ε, ∅)

    dfa.delta(a | b) should contain theSameElementsAs Seq((!abSet, Regex.∅), (abSet, ε))
    dfa.delta(ε) should contain theSameElementsAs Seq((Σ, ∅))
    dfa.delta(∅) should contain theSameElementsAs Seq((Σ, ∅))
  }

  it should "produce a DFA that has the correct structure 3" in {
    val a = Chars('a')
    val Σ = CharSet(Char.MinValue → Char.MaxValue)
    val aSet = CharSet('a')

    val dfa = DerivativeAnalysis.analyze(a.*)

    dfa.init shouldEqual (a.*)
    dfa.fin shouldEqual(Set[Regex](a.*))

    val trans : Map[Regex, Seq[(CharSet, Regex)]] = dfa.delta
    trans.keySet should contain theSameElementsAs List((a.*), ∅)

    dfa.delta(a.*) should contain theSameElementsAs Seq((!aSet, Regex.∅), (aSet, a.*))
    dfa.delta(∅) should contain theSameElementsAs Seq((Σ, ∅))
  }

  it should "produce a DFA that has the correct structure 4" in {
    val a = Chars('a')
    val Σ = CharSet(Char.MinValue → Char.MaxValue)
    val aSet = CharSet('a')

    val dfa = DerivativeAnalysis.analyze(!a)

    dfa.init shouldEqual (!a)
    dfa.fin shouldEqual(Set[Regex]((!a), (α.*)))

    val trans : Map[Regex, Seq[(CharSet, Regex)]] = dfa.delta
    trans.keySet should contain theSameElementsAs List(!(a), (α ~ (α).*), (α).*)

    dfa.delta(!a) should contain theSameElementsAs Seq((!aSet, (α).*), (aSet, α ~ (α).*))
    dfa.delta(α ~ (α).*) should contain theSameElementsAs Seq((Σ, (α).*))
  }

  it should "produce a DFA that has the correct structure 5" in {
    val a = Chars('a')
    val b = Chars('b')
    val Σ = CharSet(Char.MinValue → Char.MaxValue)
    val aSet = CharSet('a')
    val bSet = CharSet('b')

    val dfa = DerivativeAnalysis.analyze(a & b)

    dfa.init shouldEqual (a & b)
    dfa.fin shouldEqual(Set[Regex]())

    val trans : Map[Regex, Seq[(CharSet, Regex)]] = dfa.delta
    trans.keySet should contain theSameElementsAs List(a & b)

    dfa.delta(a & b) should contain theSameElementsAs Seq((Σ, a & b))
  }

  it should "produce a DFA that has the correct structure 6" in {
    val a = Chars('a')
    val b = Chars('b')
    val c = Chars('c')
    val d = Chars('d')
    val Σ = CharSet(Char.MinValue → Char.MaxValue)
    val aSet = CharSet('a')
    val bSet = CharSet('b')
    val cSet = CharSet('c')
    val dSet = CharSet('d')
    val acSet = CharSet('a' -> 'b')

    val dfa = DerivativeAnalysis.analyze(a ~ c | b ~ d)

    dfa.init shouldEqual (a ~ c | b ~ d)
    dfa.fin shouldEqual(Set[Regex](ε))

    val trans : Map[Regex, Seq[(CharSet, Regex)]] = dfa.delta
    trans.keySet should contain theSameElementsAs List(a ~ c | b ~ d, c, d, ε, ∅)

    dfa.delta(a ~ c | b ~ d) should contain theSameElementsAs Seq((!acSet, Regex.∅), (aSet, c), (bSet, d))
    dfa.delta(c) should contain theSameElementsAs Seq((!cSet, ∅), (cSet, ε))
    dfa.delta(d) should contain theSameElementsAs Seq((!dSet, ∅), (dSet, ε))
    dfa.delta(ε) should contain theSameElementsAs Seq((Σ, ∅))
    dfa.delta(∅) should contain theSameElementsAs Seq((Σ, ∅))
  }

}