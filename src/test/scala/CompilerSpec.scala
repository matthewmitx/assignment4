package edu.ucsb.cs.cs162.regex.vm.compiler

import org.scalatest._
import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.regex.vm._
import edu.ucsb.cs.cs162.range_set._

class CompileSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  val r1 = Concatenate(Chars('a'), Chars('b'))
  val r2 = Union(Chars('a'), Chars('b'))
  val r3 = KleeneStar(Chars('a'))
  val r4 = Concatenate(Chars('b'), KleeneStar(Concatenate(KleeneStar(Union(Chars('a'), Chars('c'))), KleeneStar(Chars('c')))))
  val r5 = Union(KleeneStar(Concatenate(Chars('b'), Chars('c'))), Concatenate(Chars('b'), Chars('c'))^2)
  val r6 = Concatenate(KleeneStar(Union(Chars('a').<=(3), KleeneStar(Chars('b')))), Chars('d'))
  val r7 = Intersect(Chars('a'), Chars('a'))
  val r8 = Complement(Chars('a'))

  val charsetA = CharSet('a')
  val charsetB = CharSet('b')
  val charsetC = CharSet('c')
  val charsetD = CharSet('d')
  val charsetAB = CharSet('a' -> 'b')

  import Regex._

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "compile"

  it should "correctly compile the empty language" in {
    Compiler.compile(∅) should equal (IndexedSeq(Reject, Accept))
  }

  it should "correctly compile ε" in {
    Compiler.compile(ε) should equal (IndexedSeq(PushEmpty, Accept))
  }

  it should "correctly compile concatenation" in  { 
    Compiler.compile(r1) should equal (IndexedSeq(MatchSet(charsetA), PushChar, MatchSet(charsetB), PushChar, PushConcat, Accept))
    Compiler.compile(Chars('a') ~ r1) should equal (IndexedSeq(MatchSet(charsetA), PushChar, MatchSet(charsetA), PushChar, MatchSet(charsetB), PushChar, PushConcat, PushConcat, Accept))
    Compiler.compile(Chars('a') ~ EmptyString ~ Chars('a')) should equal (IndexedSeq(MatchSet(charsetA), PushChar, MatchSet(charsetA), PushChar, PushConcat, Accept))
  }

  it should "correctly compile union" in  { 
    Compiler.compile(r2) should equal (IndexedSeq(Fork(1,5), MatchSet(charsetA), PushChar, PushLeft, Jump(4), MatchSet(charsetB), PushChar, PushRight, Accept))
    Compiler.compile(Union(EmptyString, Chars('a'))) should equal (IndexedSeq(Fork(1,4), PushEmpty, PushLeft, Jump(4), MatchSet(charsetA), PushChar, PushRight, Accept))
    Compiler.compile(Chars('a') | Chars('b')) should equal (IndexedSeq(MatchSet((charsetAB)), PushChar, Accept))
  }

  it should "correctly compile kleene star" in  { 
    Compiler.compile(r3) should equal (IndexedSeq(InitStar, Fork(1,5), MatchSet(charsetA), PushChar, PushStar, Jump(-4), Accept))
    Compiler.compile(KleeneStar(ε)) should equal (IndexedSeq(InitStar, CheckProgress, Fork(1,4), PushEmpty, PushStar, Jump(-4), Accept))
    Compiler.compile(KleeneStar(∅)) should equal (IndexedSeq(InitStar, Fork(1,4), Reject, PushStar, Jump(-3), Accept))
  }
  
  it should "correctly compile capture" in {
    Compiler.compile(Capture("none", r1)) should equal (IndexedSeq(MatchSet(charsetA), PushChar, MatchSet(charsetB), PushChar, PushConcat, PushCapture("none"), Accept))
  }

  it should "correctly compile complex regexes 1" in { 
    Compiler.compile(KleeneStar(Concatenate(r1, r2))) should equal (IndexedSeq(InitStar, Fork(1,17), MatchSet(charsetA), PushChar, MatchSet(charsetB), PushChar, PushConcat, Fork(1,5),
    MatchSet(charsetA), PushChar, PushLeft, Jump(4), MatchSet(charsetB), PushChar, PushRight, PushConcat, PushStar, Jump(-16), Accept))
  }

  it should "correctly compile complex regexes 2" in {
    Compiler.compile(KleeneStar(Concatenate(KleeneStar(r2), r1))) should equal (IndexedSeq(InitStar, Fork(1,21), InitStar, Fork(1,11), Fork(1,5), MatchSet(charsetA), PushChar, PushLeft, Jump(4),
    MatchSet(charsetB), PushChar, PushRight, PushStar, Jump(-10), MatchSet(charsetA), PushChar, MatchSet(charsetB), PushChar, PushConcat, PushConcat, PushStar, Jump(-20), Accept))
  }

  it should "correctly compile complex regexes 3" in {
    Compiler.compile(r4) should equal (IndexedSeq(MatchSet(charsetB), PushChar, InitStar, CheckProgress, Fork(1,22), InitStar, Fork(1,11), Fork(1,5), MatchSet(charsetA), PushChar, PushLeft, Jump(4),
    MatchSet(charsetC), PushChar, PushRight, PushStar, Jump(-10), InitStar, Fork(1,5), MatchSet(charsetC), PushChar, PushStar, Jump(-4), PushConcat, PushStar, Jump(-22), PushConcat, Accept))
  }

  it should "correctly compile complex regexes 4" in {
    Compiler.compile(r5) should equal (IndexedSeq(Fork(1,12), InitStar, Fork(1,8), MatchSet(charsetB), PushChar, MatchSet(charsetC), PushChar, PushConcat, PushStar, Jump(-7), PushLeft,
    Jump(13), MatchSet(charsetB), PushChar, MatchSet(charsetC), PushChar, MatchSet(charsetB), PushChar, MatchSet(charsetC), PushChar, PushConcat, PushConcat, PushConcat, PushRight, Accept))
  }

  it should "correctly compile complex regexes 5" in {
    Compiler.compile(r6) should equal (IndexedSeq(InitStar, CheckProgress, Fork(1,41), Fork(1,31), Fork(1,4), PushEmpty, PushLeft, Jump(25), Fork(1,5), MatchSet(charsetA), PushChar, PushLeft, Jump(19),
    Fork(1,8), MatchSet(charsetA), PushChar, MatchSet(charsetA), PushChar, PushConcat, PushLeft, Jump(10), MatchSet(charsetA), PushChar, MatchSet(charsetA), PushChar, MatchSet(charsetA), PushChar, PushConcat, PushConcat, PushRight,
    PushRight, PushRight, PushLeft, Jump(8), InitStar, Fork(1,5), MatchSet(charsetB), PushChar, PushStar, Jump(-4), PushRight, PushStar, Jump(-41), MatchSet(charsetD), PushChar, PushConcat, Accept))
  }

  it should "throw assertion error for invalid regexes" in {
    an [AssertionError] should be thrownBy Compiler.compile(r7)
    an [AssertionError] should be thrownBy Compiler.compile(r8)
  }
}
