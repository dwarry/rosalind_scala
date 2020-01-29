package uk.org.warry.rosalind
import org.scalatest.FunSuite

//noinspection SpellCheckingInspection
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

  test("hamm counts the number of differences between two strings"){
    val result = Problems.hamm("GAGCCTACTAACGGGAT", "CATCGTAATGACGGCCT")

    assert(result == "7")
  }

  test("cons calculates a consensus string from a FASTA file"){
    val input = """>Rosalind_1
                  |ATCCAGCT
                  |>Rosalind_2
                  |GGGCAACT
                  |>Rosalind_3
                  |ATGGATCT
                  |>Rosalind_4
                  |AAGCAACC
                  |>Rosalind_5
                  |TTGGAACT
                  |>Rosalind_6
                  |ATGCCATT
                  |>Rosalind_7
                  |ATGGCACT""".stripMargin

    val expected = """ATGCAACT
                     |A: 5 1 0 0 5 5 0 0
                     |C: 0 0 1 4 2 0 6 1
                     |G: 1 1 6 3 0 1 0 0
                     |T: 1 5 0 0 0 1 1 6""".stripMargin

    val result = Problems.cons(input.split(raw"\r?\n").iterator)

    assert(result == expected)
  }

  test("iprb calculates the chance of a dominant") {
    val result = Probability.dominantProbability(2, 2, 2)

    assert("%1.5f".format(result) == "0.78333")
  }

  test("tran calculates the transition/transversion ratio"){
    val data = """>Rosalind_0209
               |GCAACGCACAACGAAAACCCTTAGGGACTGGATTATTTCGTGATCGTTGTAGTTATTGGA
               |AGTACGGGCATCAACCCAGTT
               |>Rosalind_2200
               |TTATCTGACAAAGAAAGCCGTCAACGGCTGGATAATTTCGCGATCGTGCTGGTTACTGGC
               |GGTACGAGTGTTCCTTTGGGT""".stripMargin
    val result = Problems.tran(data.split(raw"\r?\n").iterator)

    assert(result == "1.21428571429")
  }
}
