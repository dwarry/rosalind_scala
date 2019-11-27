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
}
