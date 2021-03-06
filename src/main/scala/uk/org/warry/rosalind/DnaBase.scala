package uk.org.warry.rosalind

import uk.org.warry.rosalind.DnaBase.DnaBase
import uk.org.warry.rosalind.RnaBase.RnaBase

/**
 * Enumeration that defines the bases present in DNA, conversions and other operations
 */
object DnaBase extends Enumeration {
  type DnaBase = Value

  val A, C, G, T = Value

  /**
   * Converts from a character to the corresponding DnaBase value.
   *
   * @param ch the character which must be one of A, C, G  or T
   * @return the DnaBase value
   * @throws IllegalArgumentException if ch is not recognised
   */
  def fromChar(ch: Char): DnaBase = ch match {
    case 'A' => DnaBase.A
    case 'C' => DnaBase.C
    case 'G' => DnaBase.G
    case 'T' => DnaBase.T
    case _ => throw new IllegalArgumentException("must be one of A,C,G or T")
  }

  /**
   * Converts a DnaBase value to a character.
   * @param base the DnaBase value.
   * @return The corresponding character.
   */
  def toChar(base: DnaBase): Char = base match {
    case A => 'A'
    case C => 'C'
    case G => 'G'
    case T => 'T'
  }

  /**
   * Converts a string of base characters to a sequence of DnaBases.
   * @param letters string containing the letters to convert to bases
   * @return an Iterator that will return the DnaBase values.
   */
  def fromString(letters: String): Iterator[DnaBase] = letters.iterator.map(fromChar)

  /**
   * Converts a sequence of DnaBases to a string containing the letters of the bases.
   * @param bases the sequence to convert.
   * @return
   */
  def toString(bases: Seq[DnaBase]): String = bases.map(toChar).mkString

  /**
   * Converts a DNA base to the equivalent RNA base.
   * @param base the DNA Base
   * @return the corresponding RNA base
   */
  def toRnaBase(base: DnaBase): RnaBase = base match {
    case DnaBase.A => RnaBase.A
    case DnaBase.C => RnaBase.C
    case DnaBase.G => RnaBase.G
    case DnaBase.T => RnaBase.U
  }

  /**
   * Counts the number of each base found in a string.
   * @param letters the DNA string to be processed
   * @return BaseCount instance containing the counts of each base.
   */
  def countBases(letters: String) : BaseCount =
    fromString(letters).foldLeft (BaseCount()) ((acc: BaseCount, base: DnaBase) => base match {
        case A => acc.copy(aCount = acc.aCount + 1)
        case C => acc.copy(cCount = acc.cCount + 1)
        case G => acc.copy(gCount = acc.gCount + 1)
        case T => acc.copy(tCount = acc.tCount + 1)
      })

  /**
   * Find the complement of a base: A <=> T, C <=> G
   * @param base the DNA base.
   * @return the complementary DNA base.
   */
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
    /*
     * The fold pushes each new value onto the intermediate list, which also
     * has the effect of reversing the order, which is just what we want in this case!
     */
    bases.foldLeft (List[DnaBase]()) ((lst, b) => complement(b) :: lst)

  def gcRatio(bases: String): Double = {
    val counts = countBases (bases)
    val gc = counts.cCount + counts.gCount
    val all = counts.aCount + counts.tCount + gc
    gc / all.toDouble
  }

  /**
   * Determines whether two bases represent a transition or a transversion.
   * @param bases the pair of bases
   * @return true if the pair represents a transition, false indicates a transversion.
   */
  def isTransition(bases: (DnaBase, DnaBase)): Boolean =
    bases match {
      case (DnaBase.A, DnaBase.G) => true
      case (DnaBase.C, DnaBase.T) => true
      case (DnaBase.G, DnaBase.A) => true
      case (DnaBase.T, DnaBase.C) => true
      case _                      => false
    }

  /**
   * Find the six Open Reading Frames for a sequence of bases: these are the three consisting of the sequence itself,
   * and the sub-sequences starting at offsets 1 and 2. The other three are the corresponding sequences derived from
   * the complement of the starting sequence
   * @param bases the sequence of DnaBases under consideration
   * @return the Open Reading Frames for the bases.
   */
  def openReadingFrames(bases: Seq[DnaBase]): List[Seq[DnaBase]] = {
    val basesList = bases.toList
    val compList = complement(bases.iterator).toList

    List(
      basesList,
      basesList.tail,
      basesList.tail.tail,
      compList,
      compList.tail,
      compList.tail.tail
    )
  }
}

/**
 * Case class for returning the number of bases in a string - as returned by the `countBases` method.
 * @param aCount Number of A bases
 * @param cCount Number of C bases
 * @param gCount Number of G bases
 * @param tCount Number of T bases
 */
case class BaseCount( aCount: Int = 0, cCount: Int = 0, gCount: Int = 0, tCount: Int = 0 ) {

  /**
   * Given a particular BaseCount instance, what is the most likely bases
   * @return The Bases with the highest count. If there are multiple bases with the same
   *         count the result will be in the order A,C,G,T.
   */
  def mostLikely(): DnaBase = {
    if(aCount >= cCount && aCount >= gCount && aCount >= tCount) DnaBase.A
    else if (cCount >= gCount && cCount >= tCount) DnaBase.C
    else if (gCount >= tCount) DnaBase.G
    else DnaBase.T
  }
}
