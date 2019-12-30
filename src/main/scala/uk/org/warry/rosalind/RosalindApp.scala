package uk.org.warry.rosalind
import java.net.URL

import scala.io.Source

object RosalindApp extends App {
  if (args.length < 2) {
    printUsage()
    System.exit(1)
  }

  val problemNumber = args(0)

  val input = getLines

  val result = Problems.solve(problemNumber, input)

  println(result)

  System.exit(0)


  def getLines: Iterator[String] = {
    args.length match {
      case 1 => io.Source.stdin.getLines()
      case 2 if args(1).startsWith("file:") =>
        val f = args(1).substring(5)
        //noinspection SourceNotClosed
        Source.fromFile(f).getLines()
      case _ => args.tail.iterator
    }
  }

  def printUsage(): Unit = {
    println("Usage: problem-number inputs*")
    println("    problem-number is the number of the Project Rosalind problem")
    println("    inputs is one or more input strings. ")
    println()
    println("The result will be written in the format specified in the problem to stdout.")
  }
}
