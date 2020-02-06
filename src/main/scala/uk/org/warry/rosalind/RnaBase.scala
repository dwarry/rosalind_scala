package uk.org.warry.rosalind

import uk.org.warry.rosalind.AminoAcid.AminoAcid
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
   * @param base the RNA base
   * @return the corresponding letter
   */
  def toChar(base: RnaBase):Char = base match {
    case A => 'A'
    case C => 'C'
    case G => 'G'
    case U => 'U'
  }

  /**
   * Converts a string containing a list of RNA Bases to the corresponding RnaBase values.
   * @param letters the characters of a rna string
   * @return an Iterator of the RNABase values
   */
  def fromString(letters: String): Iterator[RnaBase] = letters.iterator.map(fromChar)

  /**
   * Converts a sequence of RnaBase values to the corresponding string of letters.
   * @param bases sequence of RnaBases
   * @return string representing the sequences of Rna Bases
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


  /**
   * Converts an iterator of DnaBases to groups of three bases - codons which can encode an amino acid
   * @param bases an iterator of RNA Bases
   * @return iterator of Codons (three RNA bases)
   */
  def rnaStringToCodons(bases: Iterator[RnaBase]): bases.GroupedIterator[RnaBase] = bases.grouped(3).withPartial(false)

  def findProteinString(bases: Iterator[RnaBase]): Seq[List[AminoAcid]] = {
    case class Accumulator(proteinStrings: List[List[AminoAcid]],
                           inCodingRegion: Boolean,
                           currentProteinString: List[AminoAcid]) {
      def startNewProteinString(): Accumulator = Accumulator(this.proteinStrings, inCodingRegion = true, List(AminoAcid.M))

      def endCurrentProteinString(): Accumulator = {

        val newProteinString = this.currentProteinString.reverse

        // need to find any substrings that begin with M as well
        val newProteinStrings =
          (newProteinString.tails.foldLeft
             (this.proteinStrings)
             ((acc, x) => if (x.headOption.contains(AminoAcid.M)) x :: acc else acc))

        Accumulator(newProteinStrings, inCodingRegion = false, List.empty[AminoAcid])
      }

      def appendToCurrentProteinString(aa: AminoAcid): Accumulator =
        this.copy(currentProteinString = aa :: this.currentProteinString)

    }

    val res = (rnaStringToCodons(bases).foldLeft
        (Accumulator(List.empty[List[AminoAcid]], inCodingRegion = false, List.empty[AminoAcid]))
        ((acc, codon) => {
          val aa = AminoAcid.codonToAminoAcid(codon)
          aa match {
            case None              if acc.inCodingRegion  => acc.endCurrentProteinString()
            case Some(AminoAcid.M) if !acc.inCodingRegion => acc.startNewProteinString()
            case Some(nextAA)      if acc.inCodingRegion  => acc.appendToCurrentProteinString(nextAA)
            case _                                        => acc
          }
        }))

    // what happens if we end mid-protein string?
    res.proteinStrings

    /*
    if (res.currentProteinString.isEmpty)
      res.proteinStrings
    else
      res.currentProteinString.reverse :: res.proteinStrings
    */
  }
}
