package uk.org.warry.rosalind

import uk.org.warry.rosalind.DnaBase.DnaBase

/**
 * Simple object which dispatches the inputs to the correct method. When you add a new solution, add it to the match
 * expression in `solve`.
 */
object Problems {

  def solve(problemId: String, arguments: Iterator[String]): String =
    problemId match {
      case "1" => problem1(arguments.next())
      case "2" => problem2(arguments.next())
      case "3" => problem3(arguments.next())
      case _   => throw new IllegalArgumentException("Unknown problem: " + problemId)
    }


  def problem1(bases: String): String = {
    val baseCount = DnaBase.countBases(bases)
    s"${baseCount.aCount} ${baseCount.cCount} ${baseCount.gCount} ${baseCount.tCount}"
  }

  def problem2(dnaString: String): String = {
    val rna = DnaBase.fromString(dnaString).map(DnaBase.toRnaBase).mkString
    rna
  }

  def problem3(dnaString: String): String = {
    val bases: Iterator[DnaBase] = DnaBase.fromString(dnaString)
    val comp = DnaBase.complement(bases)
    comp.mkString
  }
}
