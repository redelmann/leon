/* Copyright 2009-2016 EPFL, Lausanne */

package leon.regression.verification
package purescala

import smtlib.interpreters.{CVC4Interpreter, Z3Interpreter}
import leon.solvers.SolverFactory

// If you add another regression test, make sure it contains one object whose name matches the file name
// This is because we compile all tests from each folder together.
abstract class PureScalaVerificationSuite extends VerificationSuite {

  val testDir = "regression/verification/purescala/"

  val isZ3Available = SolverFactory.hasZ3

  val isCVC4Available = SolverFactory.hasCVC4

  val opts: List[List[String]] = {
    List(
      List("--feelinglucky"),
      List("--codegen", "--evalground", "--feelinglucky"),
      List("--solvers=fairz3,enum", "--codegen", "--evalground", "--feelinglucky")
    ) ++ (
      if (isZ3Available) List(
        List("--solvers=smt-z3", "--feelinglucky")
      ) else Nil
    ) ++ (
      if (isCVC4Available) List(
        List("--solvers=smt-cvc4", "--feelinglucky")
      ) else Nil
    )
  }

}

trait PureScalaValidSuite extends PureScalaVerificationSuite {
  override def testAll() = testValid()
}

class PureScalaValidSuite1 extends PureScalaValidSuite {
  val optionVariants = List(opts(0))
}
class PureScalaValidSuite2 extends PureScalaValidSuite {
  val optionVariants = List(opts(1))
}
class PureScalaValidSuite3 extends PureScalaValidSuite {
  val optionVariants = List(opts(2))
}
class PureScalaValidSuiteZ3 extends PureScalaValidSuite {
  val optionVariants = if (isZ3Available) List(opts(3)) else Nil
}
class PureScalaValidSuiteCVC4 extends PureScalaValidSuite {
  val optionVariants = if (isCVC4Available) opts.takeRight(1) else Nil
}

class PureScalaInvalidSuite extends PureScalaVerificationSuite {
  override def testAll() = testInvalid()
  val optionVariants = opts
}
