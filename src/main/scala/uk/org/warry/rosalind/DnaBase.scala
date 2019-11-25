package uk.org.warry.rosalind

import uk.org.warry.rosalind.RnaBase.RnaBase

object DnaBase extends Enumeration {
  type DnaBase = Value
  val A, C, G, T = Value

  def fromChar(ch: Char): DnaBase = ch match {
    case 'A' => DnaBase.A
    case 'C' => DnaBase.C
    case 'G' => DnaBase.G
    case 'T' => DnaBase.T
    case _ => throw new IllegalArgumentException("must be one of A,C,G or T")
  }

  def toChar(base: DnaBase) = base match {
    case A => 'A'
    case C => 'C'
    case G => 'G'
    case T => 'T'
  }

  def fromString(letters: String): Iterator[DnaBase] = letters.iterator.map(fromChar)

  def toString(bases: Seq[DnaBase]): String = bases.map(toChar).mkString

  def toRnaBase(base: DnaBase): RnaBase = base match {
    case DnaBase.A => RnaBase.A
    case DnaBase.C => RnaBase.C
    case DnaBase.G => RnaBase.G
    case DnaBase.T => RnaBase.U
  }

  def countBases(letters: String) : BaseCount =
    fromString(letters).foldLeft (BaseCount()) ((acc: BaseCount, base: DnaBase) => base match {
        case A => acc.copy(aCount = acc.aCount + 1)
        case C => acc.copy(cCount = acc.cCount + 1)
        case G => acc.copy(gCount = acc.gCount + 1)
        case T => acc.copy(tCount = acc.tCount + 1)
      })

  def complement(base: DnaBase): DnaBase = base match {
    case A => T
    case C => G
    case G => C
    case T => A
  }

  /**
   * Produce the complement of a string of bases - this is the reversed sequence of the
   * complements of the individual bases.
   * @param bases sequence of bases to complement
   * @return the new sequence
   */
  def complement(bases: Iterator[DnaBase]): Seq[DnaBase] =
    bases.foldLeft (List[DnaBase]()) ((lst, b) => complement(b) :: lst)
}

case class BaseCount( aCount: Int = 0, cCount: Int = 0, gCount: Int = 0, tCount: Int = 0 )
