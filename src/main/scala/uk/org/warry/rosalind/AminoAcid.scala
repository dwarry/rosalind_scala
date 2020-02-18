package uk.org.warry.rosalind

import uk.org.warry.rosalind.RnaBase.RnaBase

import scala.collection.Iterator

object AminoAcid extends Enumeration {
  type AminoAcid = Value

  val A, C, D, E, F, G, H, I, K, L, M, N, P, Q, R, S, T, V, W, Y = Value

  /**
   * Converts a character to the corresponding Amino Acid code.
   * @param ch the character being converted
   * @return the AminoAcid code
   * @throws IllegalArgumentException if ch does not match an AminoAcid code.
   */
  def fromChar(ch: Char): AminoAcid = ch match {
    case 'A' => A
    case 'C' => C
    case 'D' => D
    case 'E' => E
    case 'F' => F
    case 'G' => G
    case 'H' => H
    case 'I' => I
    case 'K' => K
    case 'L' => L
    case 'M' => M
    case 'N' => N
    case 'P' => P
    case 'Q' => Q
    case 'R' => R
    case 'S' => S
    case 'T' => T
    case 'V' => V
    case 'W' => W
    case 'Y' => Y
    case _ => throw new IllegalArgumentException("Unknown Amino Acid code =" + ch)
  }

  /**
   * Converts an AminoAcid code to the corresponding character.
   * @param aa the AminoAcid code
   * @return the corresponding character
   */
  def toChar(aa: AminoAcid): Char = aa match {
    case A => 'A'
    case C => 'C'
    case D => 'D'
    case E => 'E'
    case F => 'F'
    case G => 'G'
    case H => 'H'
    case I => 'I'
    case K => 'K'
    case L => 'L'
    case M => 'M'
    case N => 'N'
    case P => 'P'
    case Q => 'Q'
    case R => 'R'
    case S => 'S'
    case T => 'T'
    case V => 'V'
    case W => 'W'
    case Y => 'Y'
  }

  /**
   * Converts a string to an iterator of Amino Acids
   * @param aaString the string to convert
   * @return the corresponding sequence of AminoAcids
   */
  def fromString(aaString: String): Iterator[AminoAcid] = aaString.iterator.map(fromChar)

  def isStartCodon(codon: Seq[RnaBase]): Boolean = {
    codon match {
      case Seq(RnaBase.A, RnaBase.U, RnaBase.G) => true
      case _ => false
    }
  }

  def isEndCodon(codon: Seq[RnaBase]): Boolean = codonToAminoAcid(codon).isEmpty

  /**
   * Translate a codon (three RnaBases) to the corresponding Amino Acid, or Stop value
   * @param codon - sequence of three RnaBases
   * @return Some(AminoAcid) or None if `codon` signifies a STOP
   */
  def codonToAminoAcid(codon: Seq[RnaBase]): Option[AminoAcid] = {
    def isUorC(b:RnaBase) = b == RnaBase.U || b == RnaBase.C

    codon match {
      case Seq(RnaBase.U, RnaBase.U, x)         => Some(if(isUorC(x)) F else L)
      case Seq(RnaBase.U, RnaBase.C, _)         => Some(S)

      case Seq(RnaBase.U, RnaBase.A, x)         => if(isUorC(x)) Some(Y) else None

      case Seq(RnaBase.U, RnaBase.G, RnaBase.U) => Some(C)
      case Seq(RnaBase.U, RnaBase.G, RnaBase.C) => Some(C)
      case Seq(RnaBase.U, RnaBase.G, RnaBase.A) => None
      case Seq(RnaBase.U, RnaBase.G, RnaBase.G) => Some(W)

      case Seq(RnaBase.C, RnaBase.U, _)         => Some(L)

      case Seq(RnaBase.C, RnaBase.C, _)         => Some(P)

      case Seq(RnaBase.C, RnaBase.A, x)         => Some(if(isUorC(x)) H else Q)

      case Seq(RnaBase.C, RnaBase.G, _)         => Some(R)

      case Seq(RnaBase.A, RnaBase.U, x)         => Some(if(x == RnaBase.G) M else I)

      case Seq(RnaBase.A, RnaBase.C, _)         => Some(T)

      case Seq(RnaBase.A, RnaBase.A, x)         => Some(if(isUorC(x)) N else K)

      case Seq(RnaBase.A, RnaBase.G, x)         => Some(if(isUorC(x)) S else R)

      case Seq(RnaBase.G, RnaBase.U, _)         => Some(V)

      case Seq(RnaBase.G, RnaBase.C, _)         => Some(A)

      case Seq(RnaBase.G, RnaBase.A, x)         => Some(if(isUorC(x)) D else E)

      case Seq(RnaBase.G, RnaBase.G, _)         => Some(G)
    }
  }

  /**
   * Converts a series of Codons to the corresponding series of AminoAcid.
   * @param codons - the sequence of codons
   * @return series of AminoAcids or Stop
   */
  def translateCodonsToProteinString(codons: Iterator[Seq[RnaBase]]): Iterator[Option[AminoAcid]] =
    codons.map(codonToAminoAcid).takeWhile({_.isDefined})

  /**
   * Converts a protein string back to a normal string
   * @param aas iterator of AminoAcids / Stop values
   * @return String containing the letters of amino acids or '.' for stop markers.
   */
  def proteinStringToString(aas: Iterator[Option[AminoAcid]]): String = {
    val chars: Iterator[Option[Char]] = aas.map({_.map(toChar)})
    chars.map({_.getOrElse('.')}).mkString
  }


  /**
   * Returns the monoistopic mass of the specified AminoAcid
   * @param aa the AminoAcid
   * @return the weight of the AminoAcid
   */
  def massOf(aa: AminoAcid): Double = aa match {
    case A =>  71.03711
    case C => 103.00919
    case D => 115.02694
    case E => 129.04259
    case F => 147.06841
    case G =>  57.02146
    case H => 137.05891
    case I => 113.08406
    case K => 128.09496
    case L => 113.08406
    case M => 131.04049
    case N => 114.04293
    case P =>  97.05276
    case Q => 128.05858
    case R => 156.10111
    case S =>  87.03203
    case T => 101.04768
    case V =>  99.06841
    case W => 186.07931
    case Y => 163.06333
  }

  def massOf(aas: Iterator[AminoAcid]): Double = aas.foldLeft (0.0) (_ + massOf(_))
}
