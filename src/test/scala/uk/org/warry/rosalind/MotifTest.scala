package uk.org.warry.rosalind

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.matchers._

//noinspection SpellCheckingInspection
class MotifTest extends FunSuite with Matchers {

  test("Match element tests for an AminoAcid") {
    val m  = Match(AminoAcid.A)

    assert(m.isMatch(AminoAcid.A))
    assert(!m.isMatch(AminoAcid.C))
  }

  test("OneOf element tests for a range of AminoAcides") {
    val oo = OneOf(AminoAcid.A, AminoAcid.C)

    assert(oo.isMatch(AminoAcid.A))
    assert(oo.isMatch(AminoAcid.C))
    assert(!oo.isMatch(AminoAcid.D))
  }

  test("NotMatch tests for any other AminoAcid"){
    val nm = NotMatch(AminoAcid.A)

    assert(nm.isMatch(AminoAcid.C))
    assert(!nm.isMatch(AminoAcid.A))
  }

  test("A Motif tests a batch of AminoAcids") {
    val m = Motif(Match(AminoAcid.A), OneOf(AminoAcid.C, AminoAcid.D), NotMatch(AminoAcid.E))

    val goodBatch1 = Array(AminoAcid.A, AminoAcid.C, AminoAcid.F)
    val goodBatch2 = Array(AminoAcid.A, AminoAcid.D, AminoAcid.G)
    val badBatch1 = Array(AminoAcid.C, AminoAcid.C, AminoAcid.C)

    assert(m.checkBatch(goodBatch1))
    assert(m.checkBatch(goodBatch2))
    assert(!m.checkBatch(badBatch1))
  }

  test("A Motif can find occurences within an AminoAcid sequence"){

    val m = Motif(Match(AminoAcid.A), OneOf(AminoAcid.C, AminoAcid.D), NotMatch(AminoAcid.E))

    val aminoAcids = Array(AminoAcid.A, AminoAcid.C, AminoAcid.F,
      AminoAcid.C, AminoAcid.C, AminoAcid.C,
      AminoAcid.A, AminoAcid.D, AminoAcid.G)

    val results = m.findAnyMatches(aminoAcids).toList

    results should be (List(0, 6))
  }
}
