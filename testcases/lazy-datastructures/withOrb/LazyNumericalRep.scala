package orb

import leon.lazyeval._
import leon.lang._
import leon.annotation._
import leon.instrumentation._
import leon.lazyeval.$._
import leon.invariant._

object DigitObject {
  sealed abstract class Digit
  case class Zero() extends Digit
  case class One() extends Digit
}

import DigitObject._
object LazyNumericalRep {

  sealed abstract class NumStream {
    val isSpine: Boolean = this match {
      case Spine(_, _, _) => true
      case _ => false
    }
    val isTip = !isSpine
  }

  case class Tip() extends NumStream
  case class Spine(head: Digit, createdWithSuspension: Bool, rear: $[NumStream]) extends NumStream

  sealed abstract class Bool
  case class True() extends Bool
  case class False() extends Bool

  /**
   * Checks whether there is a zero before an unevaluated closure
   */
  def zeroPrecedeLazy[T](q: $[NumStream]): Boolean = {
    if (q.isEvaluated) {
      q* match {
        case Spine(Zero(), _, rear) =>
          true // here we have seen a zero
        case Spine(_, _, rear) =>
          zeroPrecedeLazy(rear) //here we have not seen a zero
        case Tip() => true
      }
    } else false
  }

  /**
   * Checks whether there is a zero before a given suffix
   */
  def zeroPrecedeSuf[T](q: $[NumStream], suf: $[NumStream]): Boolean = {
    if (q != suf) {
      q* match {
        case Spine(Zero(), _, rear) => true
        case Spine(_, _, rear) =>
          zeroPrecedeSuf(rear, suf)
        case Tip() => false
      }
    } else false
  }

  /**
   * Everything until suf is evaluated. This
   * also asserts that suf should be a suffix of the list
   */
  def concreteUntil[T](l: $[NumStream], suf: $[NumStream]): Boolean = {
    if (l != suf) {
      l.isEvaluated && (l* match {
        case Spine(_, cws, tail) =>
          concreteUntil(tail, suf)
        case _ =>
          false
      })
    } else true
  }

  def isConcrete[T](l: $[NumStream]): Boolean = {
    l.isEvaluated && (l* match {
      case Spine(_, _, tail) =>
        isConcrete(tail)
      case _ => true
    })
  }

  sealed abstract class Scheds
  case class Cons(h: $[NumStream], tail: Scheds) extends Scheds
  case class Nil() extends Scheds

  def schedulesProperty[T](q: $[NumStream], schs: Scheds): Boolean = {
    schs match {
      case Cons(head, tail) =>
        head* match {
          case Spine(Zero(), _, _) => // head starts with zero
            head.isSuspension(incLazy _) &&
              concreteUntil(q, head) &&
              schedulesProperty(pushUntilCarry(head), tail)
          case _ =>
            false
        }
      case Nil() =>
        isConcrete(q)
    }
  }

  @invisibleBody
  def strongSchedsProp[T](q: $[NumStream], schs: Scheds) = {
    q.isEvaluated && {
      schs match {
        case Cons(head, tail) =>
          zeroPrecedeSuf(q, head) // zeroPrecedeSuf holds initially
        case Nil() => true
      }
    } &&
      schedulesProperty(q, schs)
  }

  /**
   * Note: if 'q' has a suspension then it would have a carry.
   */
  @invisibleBody
  def pushUntilCarry[T](q: $[NumStream]): $[NumStream] = {
    q* match {
      case Spine(Zero(), _, rear) => // if we push a carry and get back 0 then there is a new carry
        pushUntilCarry(rear)
      case Spine(_, _, rear) => // if we push a carry and get back 1 then there the carry has been fully pushed
        rear
      case Tip() =>
        q
    }
  }

  case class Number(digs: $[NumStream], schedule: Scheds) {
    val valid = strongSchedsProp(digs, schedule)
  }

  @invisibleBody
  def inc(xs: $[NumStream]): NumStream = {
    require(zeroPrecedeLazy(xs))
    xs.value match {
      case Tip() =>
        Spine(One(), False(), xs)
      case s @ Spine(Zero(), _, rear) =>
        Spine(One(), False(), rear)
      case s @ Spine(_, _, _) =>
        incLazy(xs)
    }
  } ensuring (_ => time <= ?)

  @invisibleBody
  @invstate
  def incLazy(xs: $[NumStream]): NumStream = {
    require(zeroPrecedeLazy(xs) &&
      (xs* match {
        case Spine(h, _, _) => h != Zero() // xs doesn't start with a zero
        case _ => false
      }))
    xs.value match {
      case Spine(head, _, rear) => // here, rear is guaranteed to be evaluated by 'zeroPrecedeLazy' invariant
        val carry = One()
        rear.value match {
          case s @ Spine(Zero(), _, srear) =>
            val tail: NumStream = Spine(carry, False(), srear)
            Spine(Zero(), False(), tail)

          case s @ Spine(_, _, _) =>
            Spine(Zero(), True(), $(incLazy(rear)))

          case t @ Tip() =>
            val y: NumStream = Spine(carry, False(), rear)
            Spine(Zero(), False(), y)
        }
    }
  } ensuring { res =>
    (res match {
      case Spine(Zero(), _, rear) =>
        (!isConcrete(xs) || isConcrete(pushUntilCarry(rear))) &&
          {
            val _ = rear.value // this is necessary to assert properties on the state in the recursive invocation (and note this cannot go first)
            rear.isEvaluated // this is a tautology
          }
      case _ =>
        false
    }) &&
      time <= ?
  }

  /**
   * Lemma:
   * forall suf. suf*.head != Zero() ^ zeroPredsSuf(xs, suf) ^ concUntil(xs.tail.tail, suf) => concUntil(push(rear), suf)
   */
  @invisibleBody
  @invstate
  def incLazyLemma[T](xs: $[NumStream], suf: $[NumStream]): Boolean = {
    require(zeroPrecedeSuf(xs, suf) &&
      (xs* match {
        case Spine(h, _, _) => h != Zero()
        case _ => false
      }) &&
      (suf* match {
        case Spine(Zero(), _, _) =>
          concreteUntil(xs, suf)
        case _ => false
      }))
    // induction scheme
    (xs* match {
      case Spine(head, _, rear) =>
        rear* match {
          case s @ Spine(h, _, _) =>
            if (h != Zero())
              incLazyLemma(rear, suf)
            else true
          case _ => true
        }
    }) &&
      // instantiate the lemma that implies zeroPrecedeLazy
      (if (zeroPredSufConcreteUntilLemma(xs, suf)) {
        // property
        (incLazy(xs) match {
          case Spine(Zero(), _, rear) =>
            concreteUntil(pushUntilCarry(rear), suf)
        })
      } else false)
  } holds

  @invisibleBody
  def incNum[T](w: Number) = {
    require(w.valid &&
      // instantiate the lemma that implies zeroPrecedeLazy
      (w.schedule match {
        case Cons(h, _) =>
          zeroPredSufConcreteUntilLemma(w.digs, h)
        case _ =>
          concreteZeroPredLemma(w.digs)
      }))
    val nq = inc(w.digs)
    val nsched = nq match {
      case Spine(Zero(), createdWithSusp, rear) =>
        if (createdWithSusp == True())
          Cons(rear, w.schedule) // this is the only case where we create a new lazy closure
        else
          w.schedule
      case _ =>
        w.schedule
    }
    val lq: $[NumStream] = nq
    (lq, nsched)
  } ensuring { res =>
    // lemma instantiations
    (w.schedule match {
      case Cons(head, tail) =>
        w.digs* match {
          case Spine(h, _, _) =>
            if (h != Zero())
              incLazyLemma(w.digs, head)
            else true
          case _ => true
        }
      case _ => true
    }) &&
      schedulesProperty(res._1, res._2) &&
      time <= ?
  }

  @invisibleBody
  def Pay[T](q: $[NumStream], scheds: Scheds): Scheds = {
    require(schedulesProperty(q, scheds) && q.isEvaluated)
    scheds match {
      case c @ Cons(head, rest) =>
        head.value match {
          case Spine(Zero(), createdWithSusp, rear) =>
            if (createdWithSusp == True())
              Cons(rear, rest)
            else
              rest
        }
      case Nil() => scheds
    }
  } ensuring { res =>
    {
      val in = inState[NumStream]
      val out = outState[NumStream]
      // instantiations for proving the scheds property
      (scheds match {
        case Cons(head, rest) =>
          concUntilExtenLemma(q, head, in, out) &&
            (head* match {
              case Spine(Zero(), _, rear) =>
                res match {
                  case Cons(rhead, rtail) =>
                    schedMonotone(in, out, rtail, pushUntilCarry(rhead)) &&
                      concUntilMonotone(rear, rhead, in, out) &&
                      concUntilCompose(q, rear, rhead)
                  case _ =>
                    concreteMonotone(in, out, rear) &&
                      concUntilConcreteExten(q, rear)
                }
            })
        case _ => true
      }) &&
        // instantiations for zeroPrecedeSuf property
        (scheds match {
          case Cons(head, rest) =>
            (concreteUntilIsSuffix(q, head) withState in) &&
              (res match {
                case Cons(rhead, rtail) =>
                  concreteUntilIsSuffix(pushUntilCarry(head), rhead) &&
                    suffixZeroLemma(q, head, rhead) &&
                    zeroPrecedeSuf(q, rhead)
                case _ =>
                  true
              })
          case _ =>
            true
        })
    } && // properties
      schedulesProperty(q, res) &&
      time <= ?
  }

  /**
   * Pushing an element to the left of the queue preserves the data-structure invariants
   */
  @invisibleBody
  def incAndPay[T](w: Number) = {
    require(w.valid)

    val (q, scheds) = incNum(w)
    val nscheds = Pay(q, scheds)
    Number(q, nscheds)

  } ensuring { res => res.valid && time <= ? }

  // monotonicity lemmas
  def schedMonotone[T](st1: Set[$[NumStream]], st2: Set[$[NumStream]], scheds: Scheds, l: $[NumStream]): Boolean = {
    require(st1.subsetOf(st2) &&
      (schedulesProperty(l, scheds) withState st1)) // here the input state is fixed as 'st1'
    //induction scheme
    (scheds match {
      case Cons(head, tail) =>
        head* match {
          case Spine(_, _, rear) =>
            concUntilMonotone(l, head, st1, st2) &&
              schedMonotone(st1, st2, tail, pushUntilCarry(head))
          case _ => true
        }
      case Nil() =>
        concreteMonotone(st1, st2, l)
    }) && (schedulesProperty(l, scheds) withState st2) //property
  } holds

  @traceInduct
  def concreteMonotone[T](st1: Set[$[NumStream]], st2: Set[$[NumStream]], l: $[NumStream]): Boolean = {
    ((isConcrete(l) withState st1) && st1.subsetOf(st2)) ==> (isConcrete(l) withState st2)
  } holds

  @traceInduct
  def concUntilMonotone[T](q: $[NumStream], suf: $[NumStream], st1: Set[$[NumStream]], st2: Set[$[NumStream]]): Boolean = {
    ((concreteUntil(q, suf) withState st1) && st1.subsetOf(st2)) ==> (concreteUntil(q, suf) withState st2)
  } holds

  // suffix predicates and  their properties (this should be generalizable)

  def suffix[T](q: $[NumStream], suf: $[NumStream]): Boolean = {
    if (q == suf) true
    else {
      q* match {
        case Spine(_, _, rear) =>
          suffix(rear, suf)
        case Tip() => false
      }
    }
  }

  def properSuffix[T](l: $[NumStream], suf: $[NumStream]): Boolean = {
    l* match {
      case Spine(_, _, rear) =>
        suffix(rear, suf)
      case _ => false
    }
  } ensuring (res => !res || (suffixDisequality(l, suf) && suf != l))

  /**
   * suf(q, suf) ==> suf(q.rear, suf.rear)
   */
  @traceInduct
  def suffixTrans[T](q: $[NumStream], suf: $[NumStream]): Boolean = {
    suffix(q, suf) ==> ((q*, suf*) match {
      case (Spine(_, _, rear), Spine(_, _, sufRear)) =>
        // 'sufRear' should be a suffix of 'rear1'
        suffix(rear, sufRear)
      case _ => true
    })
  }.holds

  /**
   * properSuf(l, suf) ==> l != suf
   */
  def suffixDisequality[T](l: $[NumStream], suf: $[NumStream]): Boolean = {
    require(properSuffix(l, suf))
    suffixTrans(l, suf) && // lemma instantiation
      ((l*, suf*) match { // induction scheme
        case (Spine(_, _, rear), Spine(_, _, sufRear)) =>
          // 'sufRear' should be a suffix of 'rear1'
          suffixDisequality(rear, sufRear)
        case _ => true
      }) && l != suf // property
  }.holds

  @traceInduct
  def suffixCompose[T](q: $[NumStream], suf1: $[NumStream], suf2: $[NumStream]): Boolean = {
    (suffix(q, suf1) && properSuffix(suf1, suf2)) ==> properSuffix(q, suf2)
  } holds

  // properties of 'concUntil'

  @traceInduct
  def concreteUntilIsSuffix[T](l: $[NumStream], suf: $[NumStream]): Boolean = {
    concreteUntil(l, suf) ==> suffix(l, suf)
  }.holds

  // properties that extend `concUntil` to larger portions of the queue

  @traceInduct
  def concUntilExtenLemma[T](q: $[NumStream], suf: $[NumStream], st1: Set[$[NumStream]], st2: Set[$[NumStream]]): Boolean = {
    ((concreteUntil(q, suf) withState st1) && st2 == st1 ++ Set(suf)) ==>
      (suf* match {
        case Spine(_, _, rear) =>
          concreteUntil(q, rear) withState st2
        case _ => true
      })
  } holds

  @traceInduct
  def concUntilConcreteExten[T](q: $[NumStream], suf: $[NumStream]): Boolean = {
    (concreteUntil(q, suf) && isConcrete(suf)) ==> isConcrete(q)
  } holds

  @traceInduct
  def concUntilCompose[T](q: $[NumStream], suf1: $[NumStream], suf2: $[NumStream]): Boolean = {
    (concreteUntil(q, suf1) && concreteUntil(suf1, suf2)) ==> concreteUntil(q, suf2)
  } holds

  // properties that relate `concUntil`, `concrete`,  `zeroPrecedeSuf` with `zeroPrecedeLazy`
  //   - these are used in preconditions to derive the `zeroPrecedeLazy` property

  @invisibleBody
  @traceInduct
  def zeroPredSufConcreteUntilLemma[T](q: $[NumStream], suf: $[NumStream]): Boolean = {
    (zeroPrecedeSuf(q, suf) && concreteUntil(q, suf)) ==> zeroPrecedeLazy(q)
  } holds

  @invisibleBody
  @traceInduct
  def concreteZeroPredLemma[T](q: $[NumStream]): Boolean = {
    isConcrete(q) ==> zeroPrecedeLazy(q)
  } holds

  // properties relating `suffix` an `zeroPrecedeSuf`

  def suffixZeroLemma[T](q: $[NumStream], suf: $[NumStream], suf2: $[NumStream]): Boolean = {
    require(suf* match {
      case Spine(Zero(), _, _) =>
        suffix(q, suf) && properSuffix(suf, suf2)
      case _ => false
    })
    suffixCompose(q, suf, suf2) && (
      // induction scheme
      if (q != suf) {
        q* match {
          case Spine(_, _, tail) =>
            suffixZeroLemma(tail, suf, suf2)
          case _ =>
            true
        }
      } else true) &&
      zeroPrecedeSuf(q, suf2) // property
  }.holds
}
