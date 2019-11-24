package uk.org.warry.rosalind

object Problems {

  def solve(problemId: String, arguments: Iterator[String]): String = {
    problemId match {
      case "1" => problem1(arguments.next())
      case _   => throw new IllegalArgumentException("Unknown problem: " + problemId)
    }
  }

  def problem1(bases: String): String = {
    val baseCount = DnaBase.countBases(bases)
    s"${baseCount.aCount} ${baseCount.cCount} ${baseCount.gCount} ${baseCount.tCount}"
  }
}
