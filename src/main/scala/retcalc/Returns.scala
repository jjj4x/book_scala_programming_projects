package retcalc

import scala.annotation.tailrec


sealed trait Returns


object Returns {
  @tailrec
  def monthlyRate(returns: Returns, month: Int): Either[RetCalcError, Double] = returns match {
    case FixedReturns(r) => Right(r / 12)

    case VariableReturns(rs) =>
      if (rs.isDefinedAt(month))
        Right(rs(month).monthlyRate)
      else
        Left(RetCalcError.ReturnMonthOutOfBounds(month, rs.size - 1))

    case OffsetReturns(rs, offset) => monthlyRate(rs, month + offset)
  }

  def fromEquityAndInflationData(
    equities: Vector[EquityData],
    inflations: Vector[InflationData],
  ): VariableReturns = {
    VariableReturns(
      equities.zip(inflations).sliding(2).collect {
        case (prevEquity, prevInflation) +: (equity, inflation) +: Vector() =>
          val inflationRate = inflation.value / prevInflation.value
          val totalReturn = (equity.value + equity.monthlyDividend) / prevEquity.value
          val realTotalReturn = totalReturn - inflationRate
          VariableReturn(equity.monthId, realTotalReturn)
      }.toVector
    )
  }
}


case class FixedReturns(
  annualRate: Double,
)
extends Returns


case class VariableReturns(
  returns: Vector[VariableReturn],
)
extends Returns {
  def fromUntil(monthIDFrom: String, monthIDUntil: String): VariableReturns = {
    VariableReturns(
      returns
        .dropWhile(_.monthID != monthIDFrom)
        .takeWhile(_.monthID != monthIDUntil)
    )
  }
}


case class VariableReturn(
  monthID: String,
  monthlyRate: Double,
)


case class OffsetReturns(
  orig: Returns,
  offset: Int,
) extends Returns
