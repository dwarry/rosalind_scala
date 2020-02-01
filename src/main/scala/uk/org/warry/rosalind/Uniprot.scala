package uk.org.warry.rosalind

import scalaj.http._
import uk.org.warry.rosalind.AminoAcid.AminoAcid

object Uniprot {

  /**
   * Retrieves an entry from the Uniprot protein website.
   *
   * The Uniprot site may return the value for the protein id directly, or it may:
   *   - redirect to a replacement entry
   *   - return >1 results if it has been redefined into several new records.
   *
   *  In the latter two cases, the results will be whatever the requested id ultimately resolves into.
   *
   * @param uniprotId the Id of the protein in the database
   * @return a sequence of pairs consisting of the title line, and the concatenated sequence of amino acid.
   */
  def get(uniprotId: String): Seq[(String, IndexedSeq[AminoAcid])] = {

    val url = s"https://www.uniprot.org/uniprot/$uniprotId.fasta"

    // follow redirects in case the original Id has been replaced.
    val response = Http(url).options(HttpOptions.followRedirects(true)).asString

    val data = response.body.split('\n')

    Fasta.processLines(data.iterator).map(x => (x._1, x._2.map(AminoAcid.fromChar)))
  }
}
