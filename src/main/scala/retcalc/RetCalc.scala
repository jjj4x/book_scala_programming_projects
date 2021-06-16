package retcalc

import retcalc.RetCalcError.MoreExpensesThanIncome

import scala.annotation.tailrec

case class RetCalcParams(
  nOfMonthsInRetirement: Int,
  netIncome: Int,
  currentExpenses: Int,
  initialCapital: Double,
)

object RetCalc {
  def nOfMonthsSaving(returns: Returns, params: RetCalcParams): Either[RetCalcError, Int] = {
    import params._

    @tailrec
    def loop(months: Int): Either[RetCalcError, Int] = {
      simulatePlan(returns, params, months) match {
        case Right((capitalAtRetirement, capitalAfterDeath)) =>
          if (capitalAfterDeath > 0.0)
            Right(months)
          else {
            loop(months + 1)
          }
        case Left(err) => Left(err)
      }

    }

    if (netIncome > currentExpenses)
      loop(0)
    else
      Left(MoreExpensesThanIncome(netIncome, currentExpenses))
  }

  def simulatePlan(
    returns: Returns,
    params: RetCalcParams,
    nOfMonthsSavings: Int,
  ): Either[RetCalcError, (Double, Double)] = {
    import params._

    for {
      capitalAtRetirement <- futureCapital(
        returns,
        nOfMonthsSavings,
        netIncome,
        currentExpenses,
        initialCapital,
      )
      capitalAfterDeath <- futureCapital(
        returns = OffsetReturns(returns, nOfMonthsSavings),
        nOfMonthsInRetirement,
        0,
        currentExpenses,
        capitalAtRetirement,
      )
    } yield (capitalAtRetirement, capitalAfterDeath)
  }

  def futureCapital(
    returns: Returns,
    nOfMonths: Int,
    netIncome: Int,
    currentExpenses: Int,
    initialCapital: Double,
  ): Either[RetCalcError, Double] = {
    val monthlySavings = netIncome - currentExpenses

    (0 until nOfMonths).foldLeft[Either[RetCalcError, Double]](Right(initialCapital)) {
      case (accumulated, month) =>
        for {
          acc <- accumulated
          monthlyRate <- Returns.monthlyRate(returns, month)
        } yield acc * (1 + monthlyRate) + monthlySavings
    }
  }
}
