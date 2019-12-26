package uk.org.warry.rosalind

object Probability {

  /*
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
   *
   *  +-- YY
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
   *
   *
   */

  def dominantProbability(homozygousDominant: Int, heterozygous: Int, homozygousRecessive: Int) = {

    def probs(k: Int, m: Int, n: Int): (Double, Double, Double) = {
      val totalPop = k + m + n

      (k.doubleValue() / totalPop, m.doubleValue() / totalPop, n.doubleValue() / totalPop)
    }

    val (p_YY, p_yY, p_yy) = probs(homozygousDominant, heterozygous, homozygousRecessive)

    val (p_yY_YY, p_yY_yY, p_yY_yy) = probs(homozygousDominant, heterozygous - 1, homozygousRecessive)

    val (p_yy_YY, p_yy_yY, p_yy_yy) = probs(homozygousDominant, heterozygous, homozygousRecessive - 1)

    p_YY +
      (p_yY * p_yY_YY) + (p_yY * p_yY_yY * 0.75) + (p_yY * p_yY_yy * 0.5) +
      (p_yy * p_yy_YY) + (p_yy * p_yy_yY * 0.5)
  }
}
