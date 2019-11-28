package uk.org.warry.rosalind

import uk.org.warry.rosalind.DnaBase.DnaBase

/**
 * Simple object which dispatches the inputs to the correct method. When you add a new solution, add it to the match
 * expression in `solve`.
 */
object Problems {

  def solve(problemId: String, arguments: Iterator[String]): String =
    // the codes for each problem are the last component of the URL for the problem statement on the Project Rosalind,
    // so "dna" is the problem found at "http://rosalind.info/problems/dna/"
    problemId match {
      case "dna" => dna(arguments.next())
      case "fib" => fib(arguments.next(), arguments.next())
      case "gc" => gc(arguments)
      case "prot" => prot(arguments.next())
      case "rna" => rna(arguments.next())
      case "revc" => revc(arguments.next())
      case _   => throw new IllegalArgumentException("Unknown problem: " + problemId)
    }


  def dna(bases: String): String = {
    val baseCount = DnaBase.countBases(bases)
    s"${baseCount.aCount} ${baseCount.cCount} ${baseCount.gCount} ${baseCount.tCount}"
  }


  def fib(nString: String, kString: String): String = {
    val n = nString.toInt
    val k = kString.toInt

    Combinatorics.fib(n, k).toString
  }

  def gc(fasta: Iterator[String]): String = {
    val lines = Fasta.processLines(fasta)
    val (title, gc) = lines.foldLeft (("", 0.0)) ((acc, line) => {
      val (title, bases) = line
      val gc = DnaBase.gcRatio(bases)

      if(gc > acc._2)
        (title, gc)
      else
        acc
    })

    f"${title}\n${gc * 100}%2.6f"
  }

  def prot(rnaString: String): String = {
    val codons = RnaBase.rnaStringToCodons(RnaBase.fromString(rnaString))
    val aas = AminoAcid.translateCodonsToProteinString(codons)
    AminoAcid.proteinStringToString(aas)
  }

  def revc(dnaString: String): String = {
    val bases: Iterator[DnaBase] = DnaBase.fromString(dnaString)
    val comp = DnaBase.complement(bases)
    comp.mkString
  }

  def rna(dnaString: String): String = {
    val rna = DnaBase.fromString(dnaString).map(DnaBase.toRnaBase).mkString
    rna
  }
}
