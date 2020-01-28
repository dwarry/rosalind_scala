package uk.org.warry.rosalind
import org.scalatest.FunSuite

class ProblemsTest extends FunSuite {

  test("Problem1 counts bases") {

    val result = Problems.dna("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")

    assert(result == "20 12 17 21")
  }


  test("Problem2: transcribing DNA to RNA"){

    val result = Problems.rna("GATGGAACTTGACTACGTAAATT")

    assert(result == "GAUGGAACUUGACUACGUAAAUU")
  }

  test("Problem 3: generating the complement of a string of DNA Bases"){
    val result = Problems.revc("AAAACCCGGT")

    assert(result == "ACCGGGTTTT")
  }

  test("prot converts an RNA string to a Protein string"){
    val result = Problems.prot("AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA")

    assert(result == "MAMAPRTEINSTRING")
  }

  test("fib calculates a modified Fibonacci series"){
    val result = Problems.fib("5", "3")
    assert(result == "19")
  }

  test("fibd includes mortality"){
    def check(a: String, b: String, expected: String): Unit = {
      val result = Problems.fibd(a, b)

      assert(result == expected)
    }

    check("6", "3", "4")

    // other test cases from the questions section of the problem. Hope they're correct!
    check("99", "16", "215182717852492203481")

    check("100", "4", "24382819596721629")

    check("70", "10", "150851612688734")

    check("150", "30", "9968924833182193029336126851509")
  }
}
