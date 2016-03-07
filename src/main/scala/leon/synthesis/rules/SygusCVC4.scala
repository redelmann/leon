/* Copyright 2009-2016 EPFL, Lausanne */

package leon
package synthesis
package rules

import solvers.sygus._

case object SygusCVC4 extends Rule("SygusCVC4") {
  def instantiateOn(implicit hctx: SearchContext, p: Problem): Traversable[RuleInstantiation] = {
    List(new RuleInstantiation(this.name) {
      def apply(hctx: SearchContext): RuleApplication = {

        val s = new CVC4SygusSolver(hctx, hctx.program, p)

        s.checkSynth() match {
          case Some(expr) =>
            RuleClosed(Solution.term(expr, isTrusted = false))
          case None =>
            RuleFailed()
        }
      }
    })
  }
}
