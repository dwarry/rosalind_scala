package uk.org.warry.rosalind

import scalaj.http._
import uk.org.warry.rosalind.AminoAcid.AminoAcid

object Uniprot {

  /**
   * Retrieves an entry from the Uniprot protein website.
   *
   * @param uniprotId the Id of the protein in the database
   * @return a pair consisting of the title line, and the concatenated sequence of amino acid.
   */
  def get(uniprotId: String): Seq[(String, IndexedSeq[AminoAcid])] = {

    val url = s"https://www.uniprot.org/uniprot/$uniprotId.fasta"

    val response = Http(url).options(HttpOptions.followRedirects(true)).asString

    val data = response.body.split('\n')

    Fasta.processLines(data.iterator).map(x => (x._1, x._2.map(AminoAcid.fromChar)))
  }
}
