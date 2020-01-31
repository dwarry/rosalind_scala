package uk.org.warry.rosalind

import uk.org.warry.rosalind.AminoAcid.AminoAcid

sealed trait MotifElement {
  def isMatch(aa: AminoAcid): Boolean
}

case class Match(aminoAcid: AminoAcid) extends MotifElement {
  override def isMatch(aa: AminoAcid): Boolean = {
    aminoAcid == aa
  }
}

case class OneOf(aminoAcids: AminoAcid*) extends MotifElement {
  override def isMatch(aa: AminoAcid): Boolean = {
    aminoAcids.contains(aa)
  }
}

case class NotMatch(aminoAcid: AminoAcid) extends MotifElement {
  override def isMatch(aa: AminoAcid): Boolean = {
    aminoAcid != aa
  }
}

case class Motif(elements: MotifElement*) {
  def checkBatch(batch: IndexedSeq[AminoAcid]): Boolean = {
    batch.size == elements.size && elements.zip(batch).forall(x => x._1.isMatch(x._2))
  }

  def findAnyMatches(aminoAcids: IndexedSeq[AminoAcid]): Iterator[Int] = {

    val indexedBatches = aminoAcids.sliding(elements.length).zipWithIndex

    val matches = indexedBatches.filter(x => checkBatch(x._1))

    matches.map(_._2)
  }
}

