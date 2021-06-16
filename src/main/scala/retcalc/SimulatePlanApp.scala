package retcalc

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import retcalc.RetCalcError.{InvalidArgument, InvalidNumber, RetCalcResult}


object SimulatePlanApp extends App {
  strMain(args) match {
    case Invalid(err) =>
      println(err)
      sys.exit(1)

    case Valid(result) =>
      println(result)
      sys.exit(0)
  }

  def strMain(args: Array[String]): Validated[String, String] = {
    if (args.length != 6) {
      """Usage:|
        |simulatePlan from,until nbOfYearsSaving nbOfYearsRetired netIncome currentExpenses initialCapital
        |
        |Example:
        |simulatePlan 1952.09,2017.09 25 40 3000 2000 10000
      |""".stripMargin.invalid
    } else {
      val allReturns = Returns.fromEquityAndInflationData(
        equities = EquityData.fromResource("sp500.tsv"),
        inflations = InflationData.fromResource("cpi.tsv"),
      )
      val vFromUntil = parseFromUntil(args(0))
      val vNbOfYearsSaving = parseInt("nbOfYearsSaving", args(1))
      val vParams = parseParams(args)

      // Then, we call the three parsing functions that we implemented
      // earlier and assign them to vFromUntil, vNbOfYearsSaving,
      // and vParams. Their types are RetCalcResult[(String, String)],
      // RetCalcResult[Int], and RetCalcResult[RetCalcParams] respectively.
      // After that, we put these three values in Tuple3, and call
      // the cats tupled function, which combines the three elements of the
      // tuple to produce RetCalcResult[((String, String), Int, RetCalcParams)].
      // At this point, we have a ValidatedNel instance containing all
      // the required parameters to call the strSimulatePlan function
      // that we implemented earlier. In this case, we need to check
      // errors sequentially—first, we validate all the arguments,
      // and then we call strSimulatePlan. If we had used Either instead
      // of ValidatedNel, we would have used flatMap to do this.
      // Fortunately, ValidatedNel provides an equivalent method in
      // the form of andThen.
      // Before the call to .leftMap, we have an expression of
      // the RetCalcResult[String] type, which is an alias for
      // Validated[NonEmptyList[RetCalcError], String].
      // However,our function must return Validated[String, String].
      // Therefore, we transform the left NonEmptyList[RetCalcError] type
      // to a string using the anonymous function passed to .leftMap.
      (vFromUntil, vNbOfYearsSaving, vParams)
        .tupled
        .andThen {
          case ((from, until), nOfYearsSaving, params) =>
            strSimulatePlan(
              allReturns.fromUntil(from, until),
              nOfYearsSaving,
              params,
            )
        }
        .leftMap(nel => nel.map(_.message).toList.mkString("\n"))
    }
  }

  /*
  The function takes the parsed arguments, calls simulatePlan,
  and transforms its result into a string. In order to keep
  the same type as our parsing functions, we declare the return type
  of the function to be RetCalcResult[String].
  This is an alias for ValidatedNel[RetCalcError, String],
  but simulatePlan returns Either[RetCalcError, String].
  Fortunately, cats provide .toValidatedNel method to easily
  convert Either to ValidatedNel.
  */
  def strSimulatePlan(
    returns: Returns,
    nOfYearsSaving: Int,
    params: RetCalcParams,
  ): RetCalcResult[String] = {
    RetCalc.simulatePlan(
      returns = returns,
      params = params,
      nOfMonthsSavings = nOfYearsSaving * 12
    ).map {
      case (capitalAtRetirement, capitalAfterDeath) =>
        val nOfYearsInRetirement = params.nOfMonthsInRetirement / 12
        s"""|
           |Capital after $nOfYearsSaving years of savings:    ${capitalAtRetirement.round}
           |Capital after $nOfYearsInRetirement years in retirement: ${capitalAfterDeath.round}
        |""".stripMargin
    }.toValidatedNel
  }

  def parseInt(name: String, value: String): RetCalcResult[Int] = {
    Validated
      .catchOnly[NumberFormatException](value.toInt)
      .leftMap(_ => NonEmptyList.of(InvalidNumber(name, value)))
  }

  def parseFromUntil(fromUntil: String): RetCalcResult[(String, String)] = {
    val arr = fromUntil.split(",")
    if (arr.length != 2)
      InvalidArgument("fromUntil", fromUntil, "from,until").invalidNel
    else
      (arr(0), arr(1)).validNel
  }

  /*
  The function assumes that the args array has at least six elements.
  We create a tuple of four elements, each element being the result of parseInt,
  and hence, it has the RetCalcResult[Int] type. Then, we call the mapN method on our Tuple4,
  which will accumulate any error produced by the calls to parseInt.
  If all of the parseInt calls return a Valid value, the anonymous function passed to mapN is called.
  It takes Tuple4 (Int, Int, Int, Int) and returns a RetCalcParams instance.
  */
  def parseParams(args: Array[String]): RetCalcResult[RetCalcParams] = {
    (
      parseInt("nOfYearsRetired", args(2)),
      parseInt("netIncome", args(3)),
      parseInt("currentExpenses", args(4)),
      parseInt("initialCapital", args(5)),
    ).mapN {
      case (nOfYearsRetired, netIncome, currentExpenses, initialCapital) =>
        RetCalcParams(
          nOfMonthsInRetirement = nOfYearsRetired * 12,
          netIncome = netIncome,
          currentExpenses = currentExpenses,
          initialCapital = initialCapital,
        )
    }
  }
}
