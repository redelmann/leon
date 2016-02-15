/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package synthesis
package rules

import purescala.Expressions._
import purescala.Types._
import purescala.Constructors._
import purescala.Extractors._
import purescala.Common._

/**
 * Inequality Split should only be invoked on 'a' and 'b' if all we know is
 * that 'a' != 'b' in the PC, and that we don't already find witnesses of 'a' <
 * 'b' etc.. (so that the rule doesn't indefinitely apply)
 */
case object InequalitySplit extends Rule("Ineq. Split.") {
  def instantiateOn(implicit hctx: SearchContext, p: Problem): Traversable[RuleInstantiation] = {

    val TopLevelAnds(as) = and(p.pc, p.phi)

    def getNEFacts(e: Expr): Set[Set[Expr]] = e match {
      case Not(Equals(a, b)) => Set(Set(a,b))
      case _ => Set()
    }

    val neFacts = as flatMap getNEFacts

    def getFacts(e: Expr): Set[Set[Expr]] = e match {
      case GreaterEquals(a, b) => Set(Set(a,b))
      case LessEquals(a, b)    => Set(Set(a,b))
      case GreaterThan(a, b)   => Set(Set(a,b))
      case LessThan(a, b)      => Set(Set(a,b))
      case Equals(a, b)        => Set(Set(a,b))
      case _ => Set()
    }

    val facts = as flatMap getFacts


    var candidates0 = 
      (p.as.map(_.toVariable).filter(_.getType == Int32Type) :+ IntLiteral(0)).combinations(2).toList ++
      (p.as.map(_.toVariable).filter(_.getType == IntegerType) :+ InfiniteIntegerLiteral(0)).combinations(2).toList

    val candidates = candidates0.filter(neFacts contains _.toSet).filterNot(facts contains _.toSet)

    candidates.collect {
      case List(v1, v2) =>
        // v1 is always a variable, v2 may be a value
        val Variable(a1) = v1

        val pcs = List(
          GreaterThan(v1, v2),
          LessThan(v1, v2)
        )

        val subProblems = pcs.map { pc =>
          p.copy(pc = and(p.pc, pc),
                 eb = p.qeb.filterIns(pc))
        }

        val onSuccess: List[Solution] => Option[Solution] = {
          case sols @ List(sEQ, sNE) =>
          val pre = orJoin(pcs.zip(sols).map{ case (pc, sol) =>
            and(pc, sol.pre)
          })

          val term = IfExpr(pcs.head, sEQ.term, sNE.term)

          Some(Solution(pre, sols.flatMap(_.defs).toSet, term, sols.forall(_.isTrusted)))
        }

        decomp(subProblems, onSuccess, s"Ineq. Split on '$v1' and '$v2'")
    }
  }
}
