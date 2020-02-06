package uk.org.warry.rosalind

import org.scalatest.FunSuite
import org.scalatest.Matchers._
import uk.org.warry.rosalind.AminoAcid.AminoAcid

class AminoAcidTest extends FunSuite {

  test("Codons map to the expected amino acids"){

    def checkCodonMapsToExpectedAminoAcid(expected: Option[AminoAcid], codonString: String): Unit ={
      val codons = codonString.split(',')

      codons.tapEach(c => {

        val aa = AminoAcid.codonToAminoAcid(RnaBase.fromString(c).toSeq)

        val result = (expected, aa) match {
          case (None, None) => true
          case (Some(x), Some(y)) => x == y
          case _ => false
        }

        assert(result)
      })
    }

    checkCodonMapsToExpectedAminoAcid(Some(AminoAcid.A), "GCU,GCC,GCA,GCG")
    checkCodonMapsToExpectedAminoAcid(Some(AminoAcid.C), "UGU,UGC")
    checkCodonMapsToExpectedAminoAcid(Some(AminoAcid.D), "GAU,GAC")
    checkCodonMapsToExpectedAminoAcid(Some(AminoAcid.E), "GAA,GAG")
    checkCodonMapsToExpectedAminoAcid(Some(AminoAcid.F), "UUU,UUC")
    checkCodonMapsToExpectedAminoAcid(Some(AminoAcid.G), "GGU,GGC,GGA,GGG")
    checkCodonMapsToExpectedAminoAcid(Some(AminoAcid.H), "CAU,CAC")
    checkCodonMapsToExpectedAminoAcid(Some(AminoAcid.I), "AUU,AUC,AUA")
    checkCodonMapsToExpectedAminoAcid(Some(AminoAcid.K), "AAA,AAG")
    checkCodonMapsToExpectedAminoAcid(Some(AminoAcid.L), "UUA,UUG,CUU,CUC,CUA,CUG")
    checkCodonMapsToExpectedAminoAcid(Some(AminoAcid.M), "AUG")
    checkCodonMapsToExpectedAminoAcid(Some(AminoAcid.N), "AAU,AAC")
    checkCodonMapsToExpectedAminoAcid(Some(AminoAcid.P), "CCU,CCC,CCA,CCG")
    checkCodonMapsToExpectedAminoAcid(Some(AminoAcid.Q), "CAA,CAG")
    checkCodonMapsToExpectedAminoAcid(Some(AminoAcid.R), "CGU,CGC,CGA,CGG,AGA,AGG")
    checkCodonMapsToExpectedAminoAcid(Some(AminoAcid.S), "UCU,UCC,UCA,UCG,AGU,AGC")
    checkCodonMapsToExpectedAminoAcid(Some(AminoAcid.T), "ACU,ACC,ACA,ACG")
    checkCodonMapsToExpectedAminoAcid(Some(AminoAcid.V), "GUU,GUC,GUA,GUG")
    checkCodonMapsToExpectedAminoAcid(Some(AminoAcid.W), "UGG")
    checkCodonMapsToExpectedAminoAcid(Some(AminoAcid.Y), "UAU,UAC")
    checkCodonMapsToExpectedAminoAcid(None, "UAA,UGA,UAG")


  }
}
