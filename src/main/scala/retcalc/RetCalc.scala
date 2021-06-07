package retcalc

import scala.annotation.tailrec

case class RetCalcParams(
  nOfMonthsInRetirement: Int,
  netIncome: Int,
  currentExpenses: Int,
  initialCapital: Double,
)

object RetCalc {
  def nOfMonthsSaving(returns: Returns, params: RetCalcParams): Int = {
    import params._

    @tailrec
    def loop(months: Int): Int = {
      val (capitalAtRetirement, capitalAfterDeath) = simulatePlan(
        returns,
        params,
        months,
      )

      if (capitalAfterDeath > 0.0)
        months
      else
        loop(months + 1)
    }

    if (netIncome > currentExpenses)
      loop(0)
    else
      Int.MaxValue
  }

  def simulatePlan(
    returns: Returns,
    params: RetCalcParams,
    nOfMonthsSavings: Int,
  ): (Double, Double) = {
    import params._

    val capitalAtRetirement = futureCapital(returns, nOfMonthsSavings, netIncome, currentExpenses, initialCapital)
    val capitalAfterDeath = futureCapital(
      returns = OffsetReturns(returns, nOfMonthsSavings),
      nOfMonthsInRetirement,
      0,
      currentExpenses,
      capitalAtRetirement,
    )

    (capitalAtRetirement, capitalAfterDeath)
  }

  def futureCapital(
    returns: Returns,
    nOfMonths: Int,
    netIncome: Int,
    currentExpenses: Int,
    initialCapital: Double,
    ): Double = {
    val monthlySavings = netIncome - currentExpenses

    (0 until nOfMonths).foldLeft(initialCapital) {
      case (accumulated, month) =>
        accumulated * (1 + Returns.monthlyRate(returns, month)) + monthlySavings
    }
  }
}
