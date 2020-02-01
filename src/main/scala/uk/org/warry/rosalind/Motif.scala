package uk.org.warry.rosalind

import uk.org.warry.rosalind.AminoAcid.AminoAcid

/**
 * Sealed trait for Discriminated Union types which can identify components of a protein motif.
 */
sealed trait MotifElement {
  /**
   * Tests whether the specified Amino Acid is a match for being part of the motif.
   * @param aa the AminoAcid to be checked
   * @return true if the current value is a match.
   */
  def isMatch(aa: AminoAcid): Boolean
}

/**
 * Matches a specific AminoAcid.
 * @param aminoAcid the amino acid that will be consider a match
 */
case class Match(aminoAcid: AminoAcid) extends MotifElement {
  override def isMatch(aa: AminoAcid): Boolean = {
    aminoAcid == aa
  }
}

/**
 * Matches any one of the specified AminoAcid.
 * @param aminoAcids the set of amino acids that will match
 */
case class OneOf(aminoAcids: AminoAcid*) extends MotifElement {
  override def isMatch(aa: AminoAcid): Boolean = {
    aminoAcids.contains(aa)
  }
}

/**
 * Matches anything but the specified AminoAcid.
 * @param aminoAcid the amino acid that won't be considered a match.
 */
case class NotMatch(aminoAcid: AminoAcid) extends MotifElement {
  override def isMatch(aa: AminoAcid): Boolean = {
    aminoAcid != aa
  }
}

/**
 * Collection of MotifELements that can be used to identify a protein motif in a chain of Amino Acids
 * @param elements the seqeuence of MotifElements that comprise the motif.
 */
case class Motif(elements: MotifElement*) {
  /**
   * Checks if the Motif's elements match the sequence of amino acids.
   * @param batch the sequence of amino acids to be checked.
   * @return true if the batch is a match to the instance's elements.
   */
  def checkBatch(batch: IndexedSeq[AminoAcid]): Boolean = {
    // zips the motif's elements and the corresponding amino acid and checks that all the pairs match
    batch.size == elements.size && elements.zip(batch).forall(x => x._1.isMatch(x._2))
  }

  /**
   * Searches a sequence of Amino Acids for any matches for the current motif.
   * @param aminoAcids the sequence of amino acids to be searched.
   * @return zero-based indexes of any matches that are found.
   */
  def findAnyMatches(aminoAcids: IndexedSeq[AminoAcid]): Iterator[Int] = {

    // take a sliding window of the required length, and zip it with the starting index
    // so ABCDEFG being searched for a 3-element motif would give us
    // [(ABC, 0), (BCD, 1), (CDE, 2), (DEF, 3), (EFG, 4)]
    val indexedBatches = aminoAcids.sliding(elements.length).zipWithIndex

    // check each pair with the current motif and return an iterator containing those matches
    val matches = indexedBatches.filter(x => checkBatch(x._1))

    // extract the indexes from the matched pairs.
    matches.map(_._2)
  }
}

