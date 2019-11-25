package uk.org.warry.rosalind
import org.scalatest.FunSuite

class ProblemsTest extends FunSuite {

  test("Problem1 counts bases") {

    val result = Problems.problem1("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")

    assert(result == "20 12 17 21")
  }


  test("Problem2: transcribing DNA to RNA"){

    val result = Problems.problem2("GATGGAACTTGACTACGTAAATT")

    assert(result == "GAUGGAACUUGACUACGUAAAUU")
  }

  test("Problem 3: generating the complement of a string of DNA Bases"){
    val result = Problems.problem3("AAAACCCGGT")

    assert(result == "ACCGGGTTTT")
  }
}
