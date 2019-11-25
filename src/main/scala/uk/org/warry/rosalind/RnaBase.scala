package uk.org.warry.rosalind

import uk.org.warry.rosalind.DnaBase.DnaBase

object RnaBase extends Enumeration {
  type RnaBase = Value
  val A, C, G, U = Value

  def fromChar(ch: Char): RnaBase = ch match {
    case 'A' => A
    case 'C' => C
    case 'G' => G
    case 'U' => U
    case _ => throw new IllegalArgumentException("must be one of A,C,G or U")
  }

  def toChar(base: RnaBase):Char = base match {
    case A => 'A'
    case C => 'C'
    case G => 'G'
    case U => 'U'
  }

  def fromString(letters: String): Iterator[RnaBase] = letters.iterator.map(fromChar)

  def toString(bases: Seq[RnaBase]): String = bases.map(toChar).mkString

  def toDnaBase(base: RnaBase): DnaBase = base match {
    case RnaBase.A => DnaBase.A
    case RnaBase.C => DnaBase.C
    case RnaBase.G => DnaBase.G
    case RnaBase.U => DnaBase.T
  }


}
