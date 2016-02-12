package leon
package synthesis
package rules

import purescala.Expressions._
import purescala.Types._
import purescala.Extractors._
import purescala.Constructors._

/** Abstract data type split. If a variable is typed as an abstract data type, then
  * it will create a match case statement on all known subtypes. */
case object IntegerSplit extends Rule("Integer Split") {
  def instantiateOn(implicit hctx: SearchContext, p: Problem): Traversable[RuleInstantiation] = {
    // We approximate knowledge of types based on facts found at the top-level
    // we don't care if the variables are known to be equal or not, we just
    // don't want to split on two variables for which only one split
    // alternative is viable. This should be much less expensive than making
    //  calls to a solver for each pair.

    val z = InfiniteIntegerLiteral(0)

    val TopLevelAnds(as) = and(p.pc, p.phi)
    val facts = as.collect {
      case LessThan(      Variable(a), `z`) => a
      case GreaterEquals( Variable(a), `z`) => a
      case LessEquals(    Variable(a), `z`) => a
      case GreaterThan(   Variable(a), `z`) => a
      case Equals(        Variable(a), `z`) => a
    }.toSet

    p.as.collect {
      case IsTyped(id, IntegerType) if !facts(id) =>

        val subPcs = List(
          Equals(Variable(id), InfiniteIntegerLiteral(0)),
          LessThan(Variable(id), InfiniteIntegerLiteral(0)),
          GreaterThan(Variable(id), InfiniteIntegerLiteral(0))
        )

        val subProblems = subPcs map { subPc =>
          p.copy(
            pc = and(p.pc, subPc),
            eb = p.qeb.filterIns(subPc)
          )
        }

        val onSuccess: List[Solution] => Option[Solution] = {
          case sols@List(eq, lt, gt) =>
            val pres = sols.zip(subPcs).map { case (sol, sub) => and(sub, sol.pre) }
            Some(Solution(
              orJoin(pres),
              sols.flatMap(_.defs).toSet,
              IfExpr(
                subPcs(0),
                eq.term,
                IfExpr(
                  subPcs(1),
                  lt.term,
                  gt.term
                )
              )
            ))
          case _ => None
        }

        decomp(subProblems, onSuccess, s"Integer Split on '${id.asString}'")
    }
  }
}
