/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package synthesis
package rules

import leon.purescala.Common.Identifier
import purescala.Expressions._
import purescala.Extractors._
import purescala.Types._
import purescala.Constructors._

import solvers._

import scala.concurrent.duration._

/** For every pair of input variables of the same type,
  * checks equality and output an If-Then-Else statement with the two new branches. */
case object EqualitySplit extends Rule("Eq. Split") {
  def instantiateOn(implicit hctx: SearchContext, p: Problem): Traversable[RuleInstantiation] = {
    // We approximate knowledge of equality based on facts found at the top-level
    // we don't care if the variables are known to be equal or not, we just
    // don't want to split on two variables for which only one split
    // alternative is viable. This should be much less expensive than making
    // calls to a solver for each pair.
    def getFacts(e: Expr): Set[Set[Expr]] = e match {
      case Not(e) => getFacts(e)
      case LessThan(a, b)      => Set(Set(a,b))
      case LessEquals(a, b)    => Set(Set(a,b))
      case GreaterThan(a, b)   => Set(Set(a,b))
      case GreaterEquals(a, b) => Set(Set(a,b))
      case Equals(a, b)        => Set(Set(a,b))
      case _ => Set()
    }

    val TopLevelAnds(as) = and(p.pc, p.phi)

    val facts = as.flatMap(getFacts)

    var varTypes: Map[TypeTree, List[Expr]] = p.as.map(_.toVariable).groupBy(_.getType)

    varTypes += Int32Type -> (varTypes.getOrElse(Int32Type, Nil) :+ IntLiteral(0))
    varTypes += IntegerType -> (varTypes.getOrElse(IntegerType, Nil) :+ InfiniteIntegerLiteral(0))

    val candidates = varTypes.mapValues{ vs =>
      vs.combinations(2).filterNot(facts contains _.toSet)
    }.values.flatten

    candidates.flatMap {
      case List(v1, v2) =>
        // v1 is always a variable, v2 may be a value
        val Variable(a1) = v1

        val subProblems = List(
          p.copy(as  = p.as.diff(Seq(a1)),
                 pc  = subst(a1 -> v2, p.pc),
                 ws  = subst(a1 -> v2, p.ws),
                 phi = subst(a1 -> v2, p.phi),
                 eb  = p.qeb.filterIns(Equals(v1, v2)).removeIns(Set(a1))),

          p.copy(pc = and(p.pc, not(Equals(v1, v2))),
                 eb = p.qeb.filterIns(not(Equals(v1, v2))))
        )

        val onSuccess: List[Solution] => Option[Solution] = {
          case sols @ List(sEQ, sNE) =>
            val pre = or(
              and(Equals(v1, v2),      sEQ.pre),
              and(not(Equals(v1, v2)), sNE.pre)
            )

            val term = IfExpr(Equals(v1, v2), sEQ.term, sNE.term)

            Some(Solution(pre, sols.flatMap(_.defs).toSet, term, sols.forall(_.isTrusted)))
        }

        Some(decomp(subProblems, onSuccess, s"Eq. Split on '$v1' and '$v2'"))

      case _ =>
        None
    }
  }
}
