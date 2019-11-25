package uk.org.warry.rosalind

import uk.org.warry.rosalind.DnaBase.DnaBase

/**
 * Enumeration of the bases found in RNA, conversions and associated other methods.
 */
object RnaBase extends Enumeration {
  type RnaBase = Value
  val A, C, G, U = Value

  /**
   * Converts a character to the corresponding RNA Base value.
   * @param ch the character to convert
   * @return the RnaBase value
   * @throws IllegalArgumentException if ch is not one of A C G U
   */
  def fromChar(ch: Char): RnaBase = ch match {
    case 'A' => A
    case 'C' => C
    case 'G' => G
    case 'U' => U
    case _ => throw new IllegalArgumentException("must be one of A,C,G or U")
  }

  /**
   * Converts an RnaBase value to the corresponding character representation
   * @param base
   * @return
   */
  def toChar(base: RnaBase):Char = base match {
    case A => 'A'
    case C => 'C'
    case G => 'G'
    case U => 'U'
  }

  /**
   * Converts a string containing a list of RNA Bases to the corresponding RnaBase values.
   * @param letters
   * @return
   */
  def fromString(letters: String): Iterator[RnaBase] = letters.iterator.map(fromChar)

  /**
   * Converts a sequence of RnaBase values to the corresponding string of letters.
   * @param bases
   * @return
   */
  def toString(bases: Seq[RnaBase]): String = bases.map(toChar).mkString

  /**
   * Convert an RnaBase value to the corresponding DnaBase.
   * @param base the RnaBase value
   * @return The equivalent DnaBase
   */
  def toDnaBase(base: RnaBase): DnaBase = base match {
    case RnaBase.A => DnaBase.A
    case RnaBase.C => DnaBase.C
    case RnaBase.G => DnaBase.G
    case RnaBase.U => DnaBase.T
  }


}
