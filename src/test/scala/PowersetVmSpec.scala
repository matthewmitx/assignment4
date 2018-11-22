package edu.ucsb.cs.cs162.regex.vm

import org.scalatest._
import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.regex.vm.compiler._
import edu.ucsb.cs.cs162.regex.parse_tree._

class PowersetVmSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  val charA = Chars('a')
  val charB = Chars('a')
  val charC = Chars('a')
  val charD = Chars('a')

  val r1 = charA ~ charB ~ charD ~ charC
  val r2 = Union(charC, charD)
  val r3 = Union(charB, charC)
  val r4 = charA | charD
  val r5 = KleeneStar(charA)
  val r6 = KleeneStar(Concatenate(Concatenate(charA, charC), Union(charB, charC)))
  val r7 = Union(KleeneStar(Concatenate(charC, charD)), KleeneStar(Union(charA, Union(charB, charC))))
  val r8 = Concatenate(Union(charA, KleeneStar(charB)), KleeneStar(charC))

  val p1 = Compiler.compile(r1)
  val p2 = Compiler.compile(Union(r2, r3))
  val p3 = Compiler.compile(r4)
  val p4 = Compiler.compile(r5)
  val p5 = Compiler.compile(r6)
  val p6 = Compiler.compile(r7)
  val p7 = Compiler.compile(r8)

  val bvm1 = new RecursiveBacktrackingVm(p1)
  val bvm2 = new RecursiveBacktrackingVm(p2)
  val bvm3 = new RecursiveBacktrackingVm(p3)
  val bvm4 = new RecursiveBacktrackingVm(p4)
  val bvm5 = new RecursiveBacktrackingVm(p5)
  val bvm6 = new RecursiveBacktrackingVm(p6)
  val bvm7 = new RecursiveBacktrackingVm(p7)

  val vm1 = new PowersetVm(p1)
  val vm2 = new PowersetVm(p2)
  val vm3 = new PowersetVm(p3)
  val vm4 = new PowersetVm(p4)
  val vm5 = new PowersetVm(p5)
  val vm6 = new PowersetVm(p6)
  val vm7 = new PowersetVm(p7)

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "eval"

  // Replace {language 1} with a more descriptive name for what you're testing.
  // Feel free to add more tests, or write many shorter ones.

  it should "parse strings in a concatenated language" in { 
    vm1.eval("abdc") should equal (bvm1.eval("abdc"))
  }

  it should "parse strings in a unioned language" in { 
    vm2.eval("c") should equal (bvm2.eval("c"))
    vm2.eval("b") should equal (bvm2.eval("b"))
    vm3.eval("a") should equal (bvm3.eval("a"))
    vm3.eval("d") should equal (bvm3.eval("d"))
  }

  it should "parse strings in a kleenestar language" in { 
    vm4.eval("") should equal (bvm4.eval(""))
    vm4.eval("a") should equal (bvm4.eval("a"))
    vm4.eval("aaaa") should equal (bvm4.eval("aaaa"))
  }

  it should "parse strings in a complex lanuguage 1" in {
    vm5.eval("acb") should equal (bvm5.eval("acb"))
    vm5.eval("accacb") should equal (bvm5.eval("accacb"))
    vm5.eval("") should equal (bvm5.eval(""))
  }

  it should "parse strings in a complex lanuguage 2" in {
    vm6.eval("cdcdcd") should equal (bvm6.eval("cdcdcd"))
    vm6.eval("cbaabc") should equal (bvm6.eval("cbaabc"))
    vm6.eval("aaaab") should equal (bvm6.eval("aaaab"))
  }

  it should "parse strings in a complex lanuguage 3" in {
    vm7.eval("bbbcc") should equal (bvm7.eval("bbbcc"))
    vm7.eval("ac") should equal (bvm7.eval("ac"))
    vm7.eval("ccccc") should equal (bvm7.eval("ccccc"))
  }

  it should "not parse strings not in a concatenated language" in {
    vm1.eval("abdcab") should equal (bvm1.eval("abdcab"))
    vm1.eval("ab") should equal (bvm1.eval("ab"))
    vm1.eval("") should equal (bvm1.eval(""))
  }

  it should "not parse strings not in a unioned language" in {
    vm2.eval("") should equal (bvm2.eval(""))
    vm2.eval("bb") should equal (bvm2.eval("bb"))
    vm3.eval("c") should equal (bvm3.eval("c"))
    vm3.eval("ddd") should equal (bvm3.eval("ddd"))
  }

  it should "not parse strings not in a kleenestar language" in {
    vm4.eval("b") should equal (bvm4.eval("b"))
    vm4.eval("ab") should equal (bvm4.eval("ab"))
    vm4.eval("aacaa") should equal (bvm4.eval("aacaa")) 
  }

  it should "not parse strings not in a complex language 1" in {
    vm5.eval("ac") should equal (bvm5.eval("ac"))
    vm5.eval("acd") should equal (bvm5.eval("acd"))
    vm5.eval("acba") should equal (bvm5.eval("acba"))
  }

  it should "not parse strings not in a complex language 2" in {
    vm6.eval("cdcdc") should equal (bvm6.eval("cdcdc"))
    vm6.eval("aaadaaa") should equal (bvm6.eval("aaadaaa"))
    vm6.eval("d") should equal (bvm6.eval("d"))
  }

  it should "not parse strings not in a complex language 3" in {
    vm7.eval("abc") should equal (bvm7.eval("abc"))
    vm7.eval("aaac") should equal (bvm7.eval("aaac"))
    vm7.eval("bbbdc") should equal (bvm7.eval("bbbdc"))
  }

  it should "extract capture groups correctly 1" in {
    val bChar = Capture("B Character", Chars('b'))
    val cChar = Capture("C Character", Chars('c'))

    val bcUnion = Union(bChar, cChar)
    val btree = (new PowersetVm(Compiler.compile(bcUnion))).eval("b").get
    val ctree = (new PowersetVm(Compiler.compile(bcUnion))).eval("c").get

    (new Extractor(btree)).extract("B Character") shouldEqual Seq(CharLeaf('b').toString)
    (new Extractor(ctree)).extract("C Character") shouldEqual Seq(CharLeaf('c').toString)
  }

  it should "extract capture groups correctly 2" in {
    val username = (Chars('a'->'z').+).capture("username")
    val ending = (Chars('@') ~ (Chars('a'->'z').+).capture("domain") ~ Chars('.') ~
               (Chars('c') ~ Chars('o') ~ Chars('m')).capture("com")).capture("ending")
    val email = username ~ ending
    val tree = (new PowersetVm(Compiler.compile(email))).eval("imdone@finally.com").get

    (new Extractor(tree)).extract("username") shouldEqual List("imdone")
    (new Extractor(tree)).extract("ending") shouldEqual List("@finally.com")
  }

  it should "extract capture groups correctly 3" in {
    val username = (Chars('a'->'z').+).capture("username")
    val date = ((Chars('0'->'9') <= 2).capture("day") ~ Chars('/', '-') ~
               (Chars('0'->'9') <= 2).capture("month") ~ Chars('/', '-') ~
               (Chars('0'->'9') ^ 4).capture("year")).capture("date")
    val row = username ~ Chars(',') ~ date
    val tree = (new PowersetVm(Compiler.compile(row))).eval("tom,25/5/2002").get

    (new Extractor(tree)).extract("username") shouldEqual List("tom")
    (new Extractor(tree)).extract("date") shouldEqual List("25/5/2002")
  }

}
