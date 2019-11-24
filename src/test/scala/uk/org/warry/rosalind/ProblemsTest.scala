package uk.org.warry.rosalind
import org.scalatest.FunSuite

class ProblemsTest extends FunSuite {

  test("Problem1 counts bases") {

    val result = Problems.problem1("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")

    assert(result == "20 12 17 21")
  }
}
