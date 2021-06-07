package retcalc


object SimulatePlanApp extends App {
  println(strMain(args))

  def strMain(args: Array[String]): String = {
    val (from +: until +: Nil) = args(0).split(",").toList
    val nbOfYearsSaving = args(1).toInt
    val nbOfYearsInRetirement = args(2).toInt
    val allReturns = Returns.fromEquityAndInflationData(
      equities = EquityData.fromResource("sp500.tsv"),
      inflations = InflationData.fromResource("cpi.tsv"),
    )
    val (capitalAtRetirement, capitalAfterDeath) =
      RetCalc.simulatePlan(
        returns = allReturns.fromUntil(from, until),
        params = RetCalcParams(
          nOfMonthsInRetirement = nbOfYearsInRetirement * 12,
          netIncome = args(3).toInt,
          currentExpenses = args(4).toInt,
          initialCapital = args(5).toInt),
          nOfMonthsSavings = nbOfYearsSaving * 12,
      )
    s"""|
       |Capital after $nbOfYearsSaving years of savings:    ${capitalAtRetirement.round}
       |Capital after $nbOfYearsInRetirement years in retirement: ${capitalAfterDeath.round}
    |""".stripMargin
  }
}
