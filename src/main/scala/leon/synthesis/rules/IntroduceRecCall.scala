/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package synthesis
package rules

import purescala.Extractors.TopLevelAnds
import purescala.Expressions._
import purescala.Constructors._
import purescala.Common._
import Witnesses.Terminating
import utils.Helpers.terminatingCalls

case object IntroduceRecCall extends NormalizingRule("Introduce rec. calls") {
  def instantiateOn(implicit hctx: SearchContext, p: Problem): Traversable[RuleInstantiation] = {
    // val evaluator = hctx.sctx.defaultEvaluator
    val calls = terminatingCalls(hctx.program, p.ws, p.pc, None, false)
    calls.map { case (newCall, _) =>

      val rec = FreshIdentifier("rec", newCall.getType, alwaysShowUniqueID = true)
      val onSuccess = forwardMap(Let(rec, newCall, _))

      def mapExample(e: Example): List[Example] = {
        List() // FIXME: Maybe add examples if we know output
      }

      val newWs = {
        val TopLevelAnds(ws) = p.ws
        andJoin(ws filter {
          case Terminating(tfd, _) if tfd == newCall.tfd =>
            false
          case _ =>
            true
        })
      }

      // Assume the postcondition of recursive call
      val post = application(
        newCall.tfd.withParamSubst(newCall.args, newCall.tfd.postOrTrue),
        Seq(rec.toVariable)
      )

      val newProblem = p.copy(
        as = p.as :+ rec,
        pc = and(p.pc, post),
        ws = newWs,
        eb = p.eb.map(mapExample)
      )

      decomp(List(newProblem), onSuccess, s"Introduce recursive call $newCall")
    }
  }
}
