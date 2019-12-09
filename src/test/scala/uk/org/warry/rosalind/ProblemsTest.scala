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

  test("gc calculates the %age of GC bases in a string"){
    val data = """>Rosalind_6404
        |CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
        |TCCCACTAATAATTCTGAGG
        |>Rosalind_5959
        |CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
        |ATATCCATTTGTCAGCAGACACGC
        |>Rosalind_0808
        |CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
        |TGGGAACCTGCGGGCAGTAGGTGGAAT""".stripMargin

    val result = Problems.gc(data.split(raw"\r?\n").iterator)

    assert(result == "Rosalind_0808\n60.919540")
  }

  test("prtm calculates the mass of a protein string"){
    val data = "SKADYEK"
    val result = Problems.prtm(data)

    assert(result == "821.392")
  }

  test("subs find subsequences of a DnaString"){
    val result = Problems.subs("GATATATGCATATACTT", "ATAT")

    assert(result == "2 4 10")
  }
}
