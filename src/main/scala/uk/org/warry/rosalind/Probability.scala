package uk.org.warry.rosalind

object Probability {

  /**
   * NB, this problem ignores the sex of the individuals in the population.
   *
   * Punnet square for all combinations of alleles.
   *       Y  Y  |  y Y  |  y y
   *  -------------------------
   *   Y | x  x  |  X X  |  x x
   *   Y | x  x  |  x x  |  x x
   *  -------------------------
   *   Y | x  x  |  x x  |  x x
   *   y | X  X  |  . x  |  . .
   *  -------------------------
   *   y | X  X  |  . X  |  . .
   *   y | X  X  |  . X  |  . .
   *
   *  Tree of probabilities for getting offspring with the dominant allele.
   *  +-- YY    # no need to consider the next level - offspring will have a Dominant allele
   *  |
   *  |         +-- YY 1.0
   *  |         |
   * -|-- yY ---+-- yY 0.75
   *  |         |
   *  |         +-- yy 0.5
   *  |
   *  |         +-- YY 1
   *  |         |
   *  +-- yy ---+-- yY 0.5
   *            |
   *            +-- yy 0.0
   *
   * @param numHomozygousDominant  number of population with both dominant alleles
   * @param numHeterozygous        number of population with mixed alleles
   * @param numHomozygousRecessive number of popukation with both recessive alleles
   * @return probability [0, 1] of an offspring inheriting a Dominant allele
   */
  def dominantProbability(numHomozygousDominant: Int, numHeterozygous: Int, numHomozygousRecessive: Int): Double = {

    // Given the size of three fractions of a population, returns the probabilities of randomly selecting a member of
    // these fractions
    def probs(k: Int, m: Int, n: Int): (Double, Double, Double) = {
      val totalPop = k + m + n

      (k.doubleValue() / totalPop, m.doubleValue() / totalPop, n.doubleValue() / totalPop)
    }

    // Probabilities of selecting the first individual.
    val (p_YY, p_yY, p_yy) = probs(numHomozygousDominant, numHeterozygous, numHomozygousRecessive)

    // Probabilities of selecting the second individual if the first was heterozygous
    val (p_yY_YY, p_yY_yY, p_yY_yy) = probs(numHomozygousDominant, numHeterozygous - 1, numHomozygousRecessive)

    // Probabilities of selecting the second individual if the first was homzygous-recessive
    val (p_yy_YY, p_yy_yY, p_yy_yy) = probs(numHomozygousDominant, numHeterozygous, numHomozygousRecessive - 1)

    p_YY +
      (p_yY * p_yY_YY) + (p_yY * p_yY_yY * 0.75) + (p_yY * p_yY_yy * 0.5) +
      (p_yy * p_yy_YY) + (p_yy * p_yy_yY * 0.5)
  }
}
