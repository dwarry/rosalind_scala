package uk.org.warry.rosalind

object Sequences {

  def findSubsequenceIndexes[T](mainSequence: Iterator[T], subSequence: Iterator[T]): Seq[Int] = {
    val subList = subSequence.toList
    val subListLength = subList.length
    val slices = mainSequence.iterator.sliding(subListLength).withPartial(false)
    val indexedSlices = slices.zipWithIndex

    indexedSlices.foldLeft(Nil: List[Int])((matches, s) => {
      val (slice, index) = s
      val pairs = slice.zip(subList)
      if (pairs.forall(x => x._1 == x._2))
        index :: matches
      else
        matches
    })
  }
}
