package uk.org.warry.rosalind

import scala.collection.immutable._

/**
 * Methods for reading Fasta format files.
 * These consist of one or more records the consist of:
 *  - a title line which begins with '>'
 *  - one or more lines of dna base string
 */
object Fasta {

  def processLines(lines: Iterator[String]): List[(String, String)] = {

    def processLine(entries:List[(String, StringBuilder)], line: String): List[(String, StringBuilder)] = {
      if(line.startsWith(">"))
        (line.substring(1), new StringBuilder(1024)) :: entries
      else {
        entries.head._2.append(line)
        entries
      }
    }

    val result = lines.foldLeft (List[(String, StringBuilder)]()) (processLine)
    result.map(x => (x._1, x._2.toString()))
  }
}
