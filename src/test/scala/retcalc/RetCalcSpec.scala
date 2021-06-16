package retcalc

import org.scalactic.{Equality, TolerantNumerics, TypeCheckedTripleEquals}
import org.scalatest.{EitherValues, Matchers, WordSpec}


class RetCalcSpec extends WordSpec
with Matchers with TypeCheckedTripleEquals with EitherValues {
  implicit val doubleEquality: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(0.001)

  val params: RetCalcParams = RetCalcParams(
    nOfMonthsInRetirement = 40 * 12,
    netIncome = 3000,
    currentExpenses = 2000,
    initialCapital = 10000,
  )

  "RetCalc.futureCapital" should {
    "calculate the amount of savings I will have in N months" in {
      // Excel =-FV(0.04/12,25*12,1000,10000,0)
      val actual = RetCalc.futureCapital(
        returns = FixedReturns(0.04),
        nOfMonths = 25 * 12,
        netIncome = 3000,
        currentExpenses = 2000,
        initialCapital = 10000,
      ).right.value
      val expected = 541267.1990

      actual should === (expected)
    }
  }

  "calculate how much savings will be left after having taken a pension for N months" in {
    val actual = RetCalc.futureCapital(
      FixedReturns(0.04),
      nOfMonths = 40 * 12,
      netIncome = 0,
      currentExpenses = 2000,
      initialCapital = 541267.198962
    ).right.value

    val expected = 309867.53176

    actual should === (expected)
  }

  "RetCalc.simulatePlan" should {
    "calculate the capital at retirement and the capital after death" in {
      val (capitalAtRetirement, capitalAfterDeath) = RetCalc.simulatePlan(
        FixedReturns(0.04),
        params,
        25 * 12,
      ).right.value

      capitalAtRetirement should === (541267.1990)
      capitalAfterDeath should === (309867.5316)
    }

    "use different returns for capitalisation and drawdown" in {
      val nOfMonthsSavings = 25 * 12
      val returns = VariableReturns(
        Vector.tabulate(nOfMonthsSavings + params.nOfMonthsInRetirement)(
          i =>
            if (i < nOfMonthsSavings)
              VariableReturn(i.toString, 0.04 / 12)
            else
              VariableReturn(i.toString, 0.03 / 12)
        )
      )
      val (capitalAtRetirement, capitalAfterDeath) =
        RetCalc.simulatePlan(returns, params, nOfMonthsSavings).right.value

      capitalAtRetirement should ===(541267.1990)
      capitalAfterDeath should ===(-57737.7227)
    }
  }

  "RetCalc.nOfMonthsSaving" should {
    "calculate how long I need to save before I can retire" in {
      val actual = RetCalc.nOfMonthsSaving(
        FixedReturns(0.04),
        params,
      ).right.value
      val expected = 23 * 12 + 1
      actual should === (expected)
    }

    "not crash if the resulting nbOfMonths is very high" in {
      val actual = RetCalc.nOfMonthsSaving(
        FixedReturns(0.01),
        RetCalcParams(
          nOfMonthsInRetirement = 40 * 12,
          netIncome = 3000,
          currentExpenses = 2999,
          initialCapital = 0,
        ),
      ).right.value
      val expected = 8280
      actual should === (expected)
    }

    "not loop forever if I enter bad parameters" in {
      val actual = RetCalc.nOfMonthsSaving(
        FixedReturns(0.04),
        params.copy(netIncome = 1000),
      ).left.value

      // This is not very pretty, but we will see in the next chapter how we can model this better with Option or Either.
      actual should === (RetCalcError.MoreExpensesThanIncome(1000, 2000))
    }
  }

  "VariableReturns.fromUntil" should {
    "keep only a window of the returns" in {
      val variableReturns = VariableReturns(
        Vector.tabulate(12) {
          i => {
            val d = (i + 1).toDouble
            VariableReturn(f"2017.$d%02.0f", d)
          }
        }
      )

      variableReturns.fromUntil("2017.07", "2017.09").returns should === (
        Vector(VariableReturn("2017.07", 7.0), VariableReturn("2017.08", 8.0))
      )

      variableReturns.fromUntil("2017.10", "2018.01").returns should === (
        Vector(
          VariableReturn("2017.10", 10.0),
          VariableReturn("2017.11", 11.0),
          VariableReturn("2017.12", 12.0),
        )
      )
    }
  }
}
