package edu.ucsb.cs.cs162.regex

import org.scalatest._

class RegexSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  val charA = Chars('a')
  val b = Chars('b')
  val c = Chars('c')
  val d = Chars('d')
  val e = Chars('e')
  val f = Chars('f')
  val g = EmptyString
  val h = ∅

  val r = Chars('a') | Chars('b').+
  val r1 = Chars('x', 'y').* ~ r
  val r2 = Chars('y', 'x').+ ~ r
  val r3 = (e | (b | c))
  val r4 = c.*
  val r5 = e~e~e
  val r6 = !b
  val r7 = r3 & r4

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "a regex"

  it should "be buildable using `~`" in {
    (r1 ~ r2) should equal (Chars('x', 'y').* ~ r ~ Chars('y', 'x').+ ~ r)
    // simplifications
    (r ~ ∅) should equal(∅)
    (∅ ~ r) should equal(∅)
    (r ~ ε) should equal(r)
    (ε ~ r) should equal(r)
  }


  it should "be buildable using `|`" in {
    (r1 | r2) should equal(Union(r2, r1)) // also testing normalization due to lengths of r1 and r2
    // simplifications
    (r | ∅) should equal(r)
    (∅ | r) should equal(r)
    (Chars('a' -> 'c') | Chars('c' -> 'f')) should equal(Chars('a'->'f'))
    (r.* |   ε) should equal(r.*)
    (ε   | r.*) should equal(r.*)
    (α.* |   r) should equal(α.*)
    (r |   α.*) should equal(α.*)
    (r | r)     should equal(r)
  }

  it should "be buildable using `*`" in {
    r.* should equal(KleeneStar(r))
    // simplifications
    ∅.* should equal(ε)
    ε.* should equal(ε)
    (r.*).* should equal(r.*)
  }

  it should "be buildable using `!`" in {
    !r should equal(Complement(r))
    // Simplifications
    !(!r) should equal(r)
    !(∅) should equal(α.*)
    !ε should equal(α.+)
  }

  it should "be buildable using `&`" in {
    (r1 & r2) should equal(Intersect(r2, r1)) // also testing normalization due to lengths of r1 and r2
    // Simplifications
    (∅ & r) should equal(∅)
    (r & ∅) should equal(∅)
    (Chars('a'->'d') & Chars('c'->'f')) should equal (Chars('c'->'d'))
    (α.* & r) should equal(r)
    (r & α.*) should equal(r)
    (r & r) should equal(r)
  }

  it should "be buildable using `^`" in {
    (r^5) should equal(r ~ r ~ r ~ r ~ r)
  }

  it should "be buildable using `>=`" in {
    (r >= 3) should equal(r ~ r ~ r ~ r.*)
  }

  it should "be buildable using `<=`" in {
    (r <= 3) should equal(ε | r | (r ~ r) | (r ~ r ~ r))
  }

  it should "be buildable using `<>`" in {
    (r <>(2, 3)) should equal((r ~ r ~ r.*) & (ε | r | (r ~ r) | (r ~ r ~ r)))
  }


  it should "be buildable using convenience methods 1" in {
    (b ~ c) should equal (Concatenate(b, c))
  }

  it should "be buildable using convenience methods 2" in {
    (b | (b ~ c)) should equal (Union(b, Concatenate(b, c)))
  }

  it should "be buildable using convenience methods 3" in {
    b.* should equal (KleeneStar(b))
  }

  it should "be buildable using convenience methods 4" in {
    !b should equal (Complement(b))
  }

  it should "be buildable using convenience methods 5" in {
    (b & (b ~ c)) should equal (Intersect(b, Concatenate(b, c)))
  }

  it should "be buildable using convenience methods 6" in {
    b.+ should equal (Concatenate(b, KleeneStar(b)))
  }

  it should "be buildable using convenience methods 7" in {
    b.? should equal (Union(ε, b))
  }

  it should "be buildable using convenience methods 8" in {
    b^3 should equal (Concatenate(b, Concatenate(b, b)))
  }

  it should "be buildable using convenience methods 9" in {
    (b >= 2) should equal (Concatenate(b, Concatenate(b, KleeneStar(b))))
  }

  it should "be buildable using convenience methods 10" in {
    (b <= 2) should equal (Union(ε, Union(b, Concatenate(b, b))))
  }

  it should "be buildable using convenience methods 11" in {
    (b <> (1, 3)) should equal (Intersect(Concatenate(b, KleeneStar(b)), Union(ε, Union(b, Union(Concatenate(b, b), Concatenate(b, Concatenate(b, b)))))))
  }

  it should "be buildable from strings" in {
    "ab".charset ~ "cd".concatenate should equal (Concatenate(Chars('a', 'b'),
      Concatenate(Chars('c'), Chars('d'))))
  }

  it should "pretty-print correctly" in {
    (b.? | (c >= 1)).prettyPrint should equal ("""Union
                                                 |├─ ε
                                                 |└─ Union
                                                 |   ├─ b
                                                 |   └─ Concatenate
                                                 |      ├─ c
                                                 |      └─ KleeneStar
                                                 |         └─ c
                                                 |""".stripMargin)
  }

  it should "normalize correctly 1" in {
    val re = ((charA ~ b) ~ (c ~ d)) ~ (e ~ f)

    val norm = Concatenate(charA, Concatenate(b, Concatenate(c,
      Concatenate(d, Concatenate(e, f)))))

    re should equal (norm)
  }

  it should "normalize correctly 2" in {
    val re = (((b | ε) & charA) | !charA | charA.*) | ((charA ~ b) |
      charA | ε)

    val norm = Union(ε, Union(charA, Union(Concatenate(charA, b),
      Union(KleeneStar(charA), Union(Complement(charA), Intersect(charA,
        Union(ε, b)))))))

    re should equal (norm)
  }

  it should "normalize correctly 3" in { 
    val re = (((r3 ~ r4) ~ r5) ~ r6)

    val norm = Concatenate(r3, Concatenate(r4, Concatenate(e, Concatenate(e, Concatenate(e, r6)))))

    re should equal (norm)
  }

  it should "normalize correctly 4" in { 
    val re = (charA ~ b ~ c) | d | (e | f)

    val norm = Union((e | f), Union(d, Concatenate(charA, Concatenate(b, c))))

    re should equal (norm)
  }

  it should "normalize correctly 5" in { 
    val re = (b | charA) & (b | charA | c) & (b | e | charA)

    val norm = charA | b

    re should equal (norm)
  }

  it should "normalize correctly 6" in { 
    val re = f ~ e | charA ~ b

    val norm = Union(Concatenate(charA, b), Concatenate(f, e))

    re should equal (norm)
  }

  it should "normalize correctly 7" in { 
    val re = (c ~ b) ~ charA | b ~ e

    val norm = Union(Concatenate(b, e), Concatenate(c, Concatenate(b, charA)))

    re should equal (norm)
  }

  it should "normalize correctly 8" in {

    val re = (b.* & c) & b

    val norm = Intersect(b, Intersect(c, KleeneStar(b)))

    re should equal (norm)
  }



  behavior of "nullable"

  it should "recognize a nullable regex 0" in {
    (g.nullable) should equal (ε)
  }

  it should "recognize a nullable regex 1" in {
    ((r3.*).nullable) should equal (ε)
  }

  it should "recognize a nullable regex 2" in {
    (r4.nullable) should equal (ε)
  }

  it should "recognize a nullable regex 3" in {
    ((r5 | g).nullable) should equal (ε)
  }

  it should "recognize a nullable regex 4" in {
    (r6.nullable) should equal (ε)
  }

  it should "recognize a non-nullable regex 0" in {
    (h.nullable) should equal (∅)
  }

  it should "recognize a non-nullable regex 1" in {
    (b.nullable) should equal (∅)
  }

  it should "recognize a non-nullable regex 2" in {
    (r.nullable) should equal (∅)
  }

  it should "recognize a non-nullable regex 3" in {
    (r1.nullable) should equal (∅)
  }

  behavior of "assert"

  it should "throw an assertion error for bad inputs" in {

    an [AssertionError] should be thrownBy (r.^(-1))
    an [AssertionError] should be thrownBy (r.>=(-2))
    an [AssertionError] should be thrownBy (r.<=(-3))
    an [AssertionError] should be thrownBy (r.<>(-5, 4))
    an [AssertionError] should be thrownBy (r.<>(6, 2))

  }
}

