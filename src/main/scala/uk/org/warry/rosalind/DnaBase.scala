package uk.org.warry.rosalind

sealed trait DnaBase

object A extends DnaBase
object C extends DnaBase
object G extends DnaBase
object T extends DnaBase

case class BaseCount(
                      aCount: Int = 0,
                      cCount: Int = 0,
                      gCount: Int = 0,
                      tCount: Int = 0
                    )
object DnaBase {

  def fromChar(ch: Char): DnaBase = ch match {
   case 'A' => A
   case 'C' => C
   case 'G' => G
   case 'T' => T
   case _ => throw new IllegalArgumentException("must be one of A,C,G or T")
  }

  def fromString(letters: String): Iterator[DnaBase] = letters.iterator.map(fromChar)

  def countBases(letters: String) : BaseCount =
    fromString(letters).foldLeft (BaseCount()) ((acc: BaseCount, base: DnaBase) => base match {
        case A => acc.copy(aCount = acc.aCount + 1)
        case C => acc.copy(cCount = acc.cCount + 1)
        case G => acc.copy(gCount = acc.gCount + 1)
        case T => acc.copy(tCount = acc.tCount + 1)
      })
}

