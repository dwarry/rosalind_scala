package uk.org.warry.rosalind

import uk.org.warry.rosalind.AminoAcid.AminoAcid
import uk.org.warry.rosalind.DnaBase.DnaBase
import uk.org.warry.rosalind.RnaBase.RnaBase

/**
 * Simple object which dispatches the inputs to the correct method. When you add a new solution, add it to the match
 * expression in `solve`.
 */
object Problems {

  def solve(problemId: String, arguments: Iterator[String]): String =
    // the codes for each problem are the last component of the URL for the problem statement on the Project Rosalind,
    // so "dna" is the problem found at "http://rosalind.info/problems/dna/"
    problemId match {
      case "cons" => cons(arguments)
      case "dna" => dna(arguments.next())
      case "fib" => fib(arguments.next(), arguments.next())
      case "fibd" => fibd(arguments.next(), arguments.next())
      case "hamm" => hamm(arguments.next(), arguments.next())
      case "gc" => gc(arguments)
      case "iprb" => iprb(arguments.next(), arguments.next(), arguments.next())
      case "mprt" => mprt(arguments)
      case "orf" => orf(arguments)
      case "prot" => prot(arguments.next())
      case "prtm" => prtm(arguments.next())
      case "rna" => rna(arguments.next())
      case "revc" => revc(arguments.next())
      case "subs" => subs(arguments.next(), arguments.next())
      case "tran" => tran(arguments)
      case _   => throw new IllegalArgumentException("Unknown problem: " + problemId)
    }

  def cons(fasta: Iterator[String]): String = {


    def countBasesInSlices(iterators: List[Iterator[Char]]): List[BaseCount] = {
      // assuming all the iterators are of equal length, this will return
      // either the next value from all the iterators, or None if the first
      // iterator has been exhausted.
      def nextSlice(iterators: List[Iterator[Char]]): Option[List[Char]] = {
        if (iterators.head.hasNext) {
          Some(iterators.map(_.next()))
        }
        else
          None
      }

      // Recursively calculate the count of bases in each slice and build a list of the results.
      // The recursion terminates when nextSlice returns None.
      def addSlice(acc: List[BaseCount], slice: Option[List[Char]]): List[BaseCount] =
        slice match {
          case Some(x) => DnaBase.countBases(x.mkString("")) :: addSlice(acc, nextSlice(iterators))
          case None => acc
        }

      addSlice(List.empty[BaseCount], nextSlice(iterators))
    }

    val lines = Fasta.processLines(fasta)

    val bases: List[Iterator[Char]] = lines.map(_._2.iterator)

    val baseCounts: List[BaseCount] = countBasesInSlices(bases)

    val sb = new StringBuilder(1024)

    val consensus = baseCounts.map(_.mostLikely()).mkString("")

    sb.append(consensus)

    sb.append("\r\nA: ").append(baseCounts.map(_.aCount).mkString(" "))
    sb.append("\r\nC: ").append(baseCounts.map(_.cCount).mkString(" "))
    sb.append("\r\nG: ").append(baseCounts.map(_.gCount).mkString(" "))
    sb.append("\r\nT: ").append(baseCounts.map(_.tCount).mkString(" "))

    sb.result()
  }

  def dna(bases: String): String = {
    val baseCount = DnaBase.countBases(bases)
    s"${baseCount.aCount} ${baseCount.cCount} ${baseCount.gCount} ${baseCount.tCount}"
  }


  def fib(nString: String, kString: String): String = {
    val n = nString.toInt
    val k = kString.toInt

    Combinatorics.fib(n, k).toString
  }

  def fibd(nString: String, mString: String): String = {
    val n = nString.toInt
    val m = mString.toInt
    Combinatorics.fibd(n, m).toString
  }

  def gc(fasta: Iterator[String]): String = {
    val lines = Fasta.processLines(fasta)
    val (title, gc) = lines.foldLeft (("", 0.0)) ((acc, line) => {
      val (title, bases) = line
      val gc = DnaBase.gcRatio(bases)

      if(gc > acc._2)
        (title, gc)
      else
        acc
    })

    f"$title\n${gc * 100}%2.6f"
  }

  def hamm[T](a: Iterable[T], b: Iterable[T]): String = {
    val result = a.zip(b).count( x => x._1 != x._2)
    result.toString
  }

  def iprb(k: String, m: String, n: String): String = {

    val result = Probability.dominantProbability(k.toInt, m.toInt, n.toInt)

    "%1.5f".format(result)
  }

  def mprt(proteinIds: Iterator[String]): String = {
    // generate the output in the required format, appending it to a stringbuilder
    def appendResultToStringBuilder(sb: StringBuilder, entry:(String, Iterator[Int])): StringBuilder ={
      sb.append(entry._1)
      sb.append("\n")

      // need one-based results, not zero-based, so increment all the matched indexes
      val oneBasedResults = entry._2.map(_ + 1)
      sb.append(oneBasedResults.mkString(" "))
      sb.append("\n")
    }

    // just take the first result, and preserve the original Id
    val proteins = proteinIds.map(x => (x, Uniprot.get(x).head._2))

    // find any matches
    val result = mprt_core(proteins)

    // Generate the output and return the string.
    result.foldLeft (new StringBuilder()) (appendResultToStringBuilder).toString
  }

  /**
   * The actual implementation of the MPRT problem, decoupled from retrieving them from the Uniprot website, and
   * generating the output in the required format.
   * @param proteins tuples of the uniprot id and sequence of amino acids.
   * @return tuples of the uniprot id and the 0-based indexes of the motif positions.
   */
  def mprt_core(proteins: Iterator[(String, IndexedSeq[AminoAcid])]): Iterator[(String, Iterator[Int])] = {

    // Our motif in question - the N-glycosylation
    // N{P}[ST]{P}.
    val motif = Motif(
      Match(AminoAcid.N),
      NotMatch(AminoAcid.P),
      OneOf(AminoAcid.S, AminoAcid.T),
      NotMatch(AminoAcid.P))

    // check each Amino Acid sequence for the motif, and return those with matches
    proteins.map(x => (x._1, motif.findAnyMatches(x._2))).filter(x => x._2.nonEmpty)
  }

  def orf(dnaFasta: Iterator[String]): String = {

    val dna = Fasta.processLines(dnaFasta).head._2.map(DnaBase.fromChar)

    val orfRna = DnaBase.openReadingFrames(dna).map(dnaString => dnaString.map(DnaBase.toRnaBase))
    
    val proteinStrings = orfRna.flatMap(x => RnaBase.findProteinString(x.iterator))
      
    proteinStrings.map(x => x.map(AminoAcid.toChar).mkString).distinct.mkString("\n")

  }

  def prot(rnaString: String): String = {
    val codons = RnaBase.rnaStringToCodons(RnaBase.fromString(rnaString))
    val aas = AminoAcid.translateCodonsToProteinString(codons)
    AminoAcid.proteinStringToString(aas)
  }

  def prtm(aaString: String): String = {
    val aminoAcids = AminoAcid.fromString(aaString)
    AminoAcid.massOf(aminoAcids).formatted("%.3f")
  }

  def revc(dnaString: String): String = {
    val bases: Iterator[DnaBase] = DnaBase.fromString(dnaString)
    val comp = DnaBase.complement(bases)
    comp.mkString
  }

  def rna(dnaString: String): String = {
    val rna = DnaBase.fromString(dnaString).map(DnaBase.toRnaBase).mkString
    rna
  }

  def subs(dnaString: String, subString: String): String = {
    val dna = DnaBase.fromString(dnaString)
    val sub = DnaBase.fromString(subString)

    val indexes = Sequences.findSubsequenceIndexes(dna, sub)

    // output needs to be 1-based and in ascending order
    indexes.map(_ + 1).reverse.mkString(" ")
  }

  def tran(fasta: Iterator[String]): String = {

    val lines = Fasta.processLines(fasta).map(x => DnaBase.fromString(x._2)).take(2)

    lines match {
      case Seq(a, b) =>
        val (numTransitions, numTransversions) = a.zip(b).foldLeft((0,0))((acc, bases) =>
          if (bases._1 == bases._2)
            acc
          else if (DnaBase.isTransition(bases))
            (acc._1 + 1, acc._2)
          else
            (acc._1, acc._2 + 1)
        )

        val ratio = numTransitions.doubleValue() / numTransversions

        "%2.11f".format(ratio)
      case _ => ""
    }
  }
}
