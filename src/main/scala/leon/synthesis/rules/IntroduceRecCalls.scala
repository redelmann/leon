/* Copyright 2009-2016 EPFL, Lausanne */

package leon
package synthesis
package rules

import evaluators.DefaultEvaluator
import purescala.Definitions.Program
import purescala.Extractors.TopLevelAnds
import purescala.Expressions._
import purescala.Constructors._
import purescala.Common._
import Witnesses.Terminating
import utils.Helpers.terminatingCalls

case object IntroduceRecCalls extends NormalizingRule("Introduce rec. calls") {

  private class NoChooseEvaluator(ctx: LeonContext, prog: Program) extends DefaultEvaluator(ctx, prog) {
    override def e(expr: Expr)(implicit rctx: RC, gctx: GC): Expr = expr match {
      case ch: Choose =>
        throw EvalError("Encountered choose!")
      case _ =>
        super.e(expr)
    }
  }

  def instantiateOn(implicit hctx: SearchContext, p: Problem): Traversable[RuleInstantiation] = {

    val (orig, calls, _) = terminatingCalls(hctx.program, p.ws, p.pc, None, false).unzip3

    if (calls.isEmpty) return Nil

    val specifyCalls = hctx.findOptionOrDefault(SynthesisPhase.optSpecifyRecCalls)

    val (recs, posts) = calls.map { newCall =>
      val rec = FreshIdentifier("rec", newCall.getType, alwaysShowUniqueID = true)

      // Assume the postcondition of recursive call
      val post = if (specifyCalls) {
        Equals(rec.toVariable, newCall)
      } else {
        application(
          newCall.tfd.withParamSubst(newCall.args, newCall.tfd.postOrTrue),
          Seq(rec.toVariable)
        )
      }
      (rec, post)
    }.unzip

    val onSuccess = forwardMap(letTuple(recs, tupleWrap(calls), _))

    List(new RuleInstantiation(s"Introduce recursive calls ${calls mkString ", "}", SolutionBuilderDecomp(List(p.outType), onSuccess)) {

      def apply(nohctx: SearchContext): RuleApplication = {

        val psol = new PartialSolution(hctx.search.strat, true)
          .solutionAround(hctx.currentNode)(Error(p.outType, "Encountered choose!"))
          .getOrElse(hctx.reporter.fatalError("Unable to get outer solution"))
          .term

        val origImpl = hctx.functionContext.fullBody
        hctx.functionContext.fullBody = psol

        val evaluator = new NoChooseEvaluator(hctx, hctx.program)
        def mapExample(ex: Example): List[Example] = {
          val results = calls map (evaluator.eval(_, p.as.zip(ex.ins).toMap).result)
          if (results forall (_.isDefined)) List({
            val extra = results map (_.get)
            ex match {
              case InExample(ins) =>
                InExample(ins ++ extra)
              case InOutExample(ins, outs) =>
                InOutExample(ins ++ extra, outs)
            }
          }) else Nil
        }

        val newWs = calls map Terminating

        val TopLevelAnds(ws) = p.ws

        try {
          val newProblem = p.copy(
            as = p.as ++ recs,
            pc = andJoin(p.pc +: posts),
            ws = andJoin((ws diff orig) ++ newWs),
            eb = p.eb.map(mapExample)
          )

          RuleExpanded(List(newProblem))
        } finally {
          hctx.functionContext.fullBody = origImpl
        }
      }
    })

  }
}
