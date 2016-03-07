/* Copyright 2009-2016 EPFL, Lausanne */

package leon
package invariant.engine

import purescala.Common._
import purescala.Definitions._
import purescala.Expressions._
import purescala.ExprOps._
import purescala.Types._
import purescala.DefOps._
import purescala.ScalaPrinter

import solvers._
import verification._
import invariant.factories._
import invariant.util._
import invariant.structure._
import transformations._
import FunctionUtils._
import Util._
import PredicateUtil._
import ProgramUtil._

/**
 * @author ravi
 * This phase performs automatic invariant inference.
 * TODO: Do we need to also assert that time is >= 0
 */
case class InferResult(res: Boolean, model: Option[Model], inferredFuncs: List[FunDef]) {
}

trait FunctionTemplateSolver {
  def apply(): Option[InferResult]
}

class UnfoldingTemplateSolver(ctx: InferenceContext, program: Program, rootFd: FunDef) extends FunctionTemplateSolver {

  val reporter = ctx.reporter
  val debugVCs = false

  lazy val constTracker = new ConstraintTracker(ctx, program, rootFd)
  lazy val templateSolver = TemplateSolverFactory.createTemplateSolver(ctx, program, constTracker, rootFd)

  def constructVC(funDef: FunDef): (Expr, Expr) = {
    val body = funDef.body.get
    val Lambda(Seq(ValDef(resid)), _) = funDef.postcondition.get
    val resvar = resid.toVariable

    val simpBody = matchToIfThenElse(body)
    val plainBody = Equals(resvar, simpBody)
    val bodyExpr = if (funDef.hasPrecondition) {
      And(matchToIfThenElse(funDef.precondition.get), plainBody)
    } else plainBody

    val funName = fullName(funDef, useUniqueIds = false)(program)
    val fullPost = matchToIfThenElse(
      if (funDef.hasTemplate) {
        // if the postcondition is verified do not include it in the sequent
        if (ctx.isFunctionPostVerified(funName))
          funDef.getTemplate
        else
          And(funDef.getPostWoTemplate, funDef.getTemplate)
      } else if (!ctx.isFunctionPostVerified(funName))
        funDef.getPostWoTemplate
      else
        BooleanLiteral(true))

    (bodyExpr, fullPost)
  }

  def solveParametricVC(vc: Expr) = {
    val vcExpr = ExpressionTransformer.normalizeExpr(vc, ctx.multOp)
    //for debugging
    if (debugVCs) reporter.info("flattened VC: " + ScalaPrinter(vcExpr))

    // initialize the constraint tracker
    constTracker.addVC(rootFd, vcExpr)

    var refinementStep: Int = 0
    var toRefineCalls: Option[Set[Call]] = None
    var infRes: Option[InferResult] = None
    do {
      infRes =
        if(ctx.abort)
          Some(InferResult(false, None, List()))
        else{
          Stats.updateCounter(1, "VC-refinement")
          /*
           * uncomment if we want to bound refinements
           * if (refinementStep >= 5)
           * throw new IllegalStateException("Done 4 refinements")
           */
          val refined =
            if (refinementStep >= 1) {
              reporter.info("- More unrollings for invariant inference")

              val toUnrollCalls = if (ctx.targettedUnroll) toRefineCalls else None
              val unrolledCalls = constTracker.refineVCs(toUnrollCalls)
              if (unrolledCalls.isEmpty) {
                reporter.info("- Cannot do more unrollings, reached unroll bound")
                false
              } else true
            } else {
              constTracker.initialize
              true
            }
          refinementStep += 1
          if (!refined)
            Some(InferResult(false, None, List()))
          else {
            //solve for the templates in this unroll step
            templateSolver.solveTemplates() match {
              case (Some(model), callsInPath) =>
                toRefineCalls = callsInPath
                //Validate the model here
                instantiateAndValidateModel(model, constTracker.getFuncs)
                Some(InferResult(true, Some(model),
                  constTracker.getFuncs.toList))
              case (None, callsInPath) =>
                toRefineCalls = callsInPath
                //here, we do not know if the template is solvable or not, we need to do more unrollings.
                None
            }
          }
        }
    } while (infRes.isEmpty)
    infRes
  }

  def apply() = {
    if(ctx.abort) {
      Some(InferResult(false, None, List()))
    } else {
      //create a body and post of the function
      val (bodyExpr, fullPost) = constructVC(rootFd)
      if (fullPost == tru)
        Some(InferResult(true, Some(Model.empty), List()))
      else
        solveParametricVC(And(bodyExpr, Not(fullPost)))
    }
  }

  def instantiateModel(model: Model, funcs: Seq[FunDef]) = {
    funcs.collect {
      case fd if fd.hasTemplate =>
        fd -> fd.getTemplate
    }.toMap
  }

  def instantiateAndValidateModel(model: Model, funcs: Seq[FunDef]) = {
    val templates = instantiateModel(model, funcs)
    val sols = TemplateInstantiator.getAllInvariants(model, templates)

    var output = "Invariants for Function: " + rootFd.id + "\n"
    sols foreach {
      case (fd, inv) =>
        val simpInv = simplifyArithmetic(InstUtil.replaceInstruVars(multToTimes(inv), fd))
        reporter.info("- Found inductive invariant: " + fd.id + " --> " + ScalaPrinter(simpInv))
        output += fd.id + " --> " + simpInv + "\n"
    }
    SpecificStats.addOutput(output)

    reporter.info("- Verifying Invariants... ")
    val verifierRes = verifyInvariant(sols)
    val finalRes = verifierRes._1 match {
      case Some(false) =>
        reporter.info("- Invariant verified")
        sols
      case Some(true) =>
        reporter.error("- Invalid invariant, model: " + verifierRes._2)
        throw new IllegalStateException("")
      case _ =>
        //the solver timed out here
        reporter.error("- Unable to prove or disprove invariant, the invariant is probably true")
        sols
    }
    finalRes
  }

  /**
   * This function creates a new program with each function postcondition strengthened by
   * the inferred postcondition
   */
  def verifyInvariant(newposts: Map[FunDef, Expr]): (Option[Boolean], Model) = {
    //create a fundef for each function in the program
    //note: mult functions are also copied
    val newFundefs = program.definedFunctions.collect {
      case fd @ _ => { //if !isMultFunctions(fd)
        val newfd = new FunDef(FreshIdentifier(fd.id.name, Untyped, false), fd.tparams, fd.params, fd.returnType)
        (fd, newfd)
      }
    }.toMap
    //note:  we are not replacing "mult" function by "Times"
    val replaceFun = (e: Expr) => e match {
      case fi @ FunctionInvocation(tfd1, args) if newFundefs.contains(tfd1.fd) =>
        FunctionInvocation(TypedFunDef(newFundefs(tfd1.fd), tfd1.tps), args)
      case _ => e
    }
    //create a body, pre, post for each newfundef
    newFundefs.foreach((entry) => {
      val (fd, newfd) = entry
      //add a new precondition
      newfd.precondition =
        if (fd.precondition.isDefined)
          Some(simplePostTransform(replaceFun)(fd.precondition.get))
        else None

      //add a new body
      newfd.body = if (fd.hasBody)
        Some(simplePostTransform(replaceFun)(fd.body.get))
      else None

      //add a new postcondition
      val newpost = if (newposts.contains(fd)) {
        val inv = newposts(fd)
        if (fd.postcondition.isDefined) {
          val Lambda(resultBinder, _) = fd.postcondition.get
          Some(Lambda(resultBinder, And(fd.getPostWoTemplate, inv)))
        } else {
          //replace #res in the invariant by a new result variable
          val resvar = FreshIdentifier("res", fd.returnType, true)
          // FIXME: Is this correct (ResultVariable(fd.returnType) -> resvar.toVariable))
          val ninv = replace(Map(ResultVariable(fd.returnType) -> resvar.toVariable), inv)
          Some(Lambda(Seq(ValDef(resvar)), ninv))
        }
      } else if (fd.postcondition.isDefined) {
        val Lambda(resultBinder, _) = fd.postcondition.get
        Some(Lambda(resultBinder, fd.getPostWoTemplate))
      } else None

      newfd.postcondition = if (newpost.isDefined) {
        val Lambda(resultBinder, pexpr) = newpost.get
        // Some((resvar, simplePostTransform(replaceFun)(pexpr)))
        Some(Lambda(resultBinder, simplePostTransform(replaceFun)(pexpr)))
      } else None
      newfd.addFlags(fd.flags)
    })

    val augmentedProg = copyProgram(program, (defs: Seq[Definition]) => defs.collect {
      case fd: FunDef if (newFundefs.contains(fd)) => newFundefs(fd)
      case d if (!d.isInstanceOf[FunDef]) => d
    })
    //convert the program back to an integer program if necessary
    val (newprog, newroot) = if (ctx.usereals) {
      val realToIntconverter = new RealToIntProgram()
      val intProg = realToIntconverter(augmentedProg)
      (intProg, realToIntconverter.mappedFun(newFundefs(rootFd)))
    } else {
      (augmentedProg, newFundefs(rootFd))
    }
    //println("New Root: "+newroot)
    import leon.solvers.smtlib.SMTLIBZ3Solver
    import leon.solvers.combinators.UnrollingSolver
    val dummySolFactory = new leon.solvers.SolverFactory[SMTLIBZ3Solver] {
      def getNewSolver() = new SMTLIBZ3Solver(ctx.leonContext, program)
    }
    val vericontext = VerificationContext(ctx.leonContext, newprog, dummySolFactory, reporter)
    val defaultTactic = new DefaultTactic(vericontext)
    val vc = defaultTactic.generatePostconditions(newroot)(0)

    val verifyTimeout = 5
    //    val fairZ3 = new SimpleSolverAPI(
    //      new TimeoutSolverFactory(SolverFactory(() =>
    //        new FairZ3Solver(ctx.leonContext, newprog) with TimeoutSolver),
    //        verifyTimeout * 1000))
    val smtUnrollZ3 = new UnrollingSolver(ctx.leonContext, program,
      new SMTLIBZ3Solver(ctx.leonContext, program)) with TimeoutSolver
    smtUnrollZ3.setTimeout(verifyTimeout * 1000)
    smtUnrollZ3.assertVC(vc)
    smtUnrollZ3.check match {
      case Some(true) =>
        (Some(true), smtUnrollZ3.getModel)
      case r =>
        (r, Model.empty)
    }
  }
}
