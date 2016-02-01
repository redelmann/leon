/* Copyright 2009-2015 EPFL, Lausanne */
package leon

import leon.annotation._
import leon.lang._

import java.util._
import java.util.concurrent._
import scala.util.DynamicVariable

package object parallel {

  @ignore
  val forkJoinPool = new ForkJoinPool

  @ignore
  abstract class TaskScheduler {
    def schedule[T](body: => T): concurrent.ForkJoinTask[T]

    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      val right = task {
        taskB
      }
      val left = taskA
      (left, right.join())
    }
  }

  @ignore
  class DefaultTaskScheduler extends TaskScheduler {
    def schedule[T](body: => T): concurrent.ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute = body
      }
      forkJoinPool.execute(t)
      t
    }
  }

  @ignore
  val scheduler =
    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

  /**
   * A leon stub for concurrent.ForkJoinTask
   * Implements only a subset of operations supported by concurrent.ForkJoinTask.
   * Uses mutation to allow leon parse the class.
   */
  case class ForkJoinTask[T]() {
    @ignore
    var task: concurrent.ForkJoinTask[T] = null

    @extern
    def join() = task.join
  }

  @extern
  def task[T](body: => T): ForkJoinTask[T] = {
    val fjt = ForkJoinTask[T]()
    fjt.task = scheduler.value.schedule(body)
    fjt
  }

  @extern
  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    scheduler.value.parallel(taskA, taskB)
  }

  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
    val ta = task { taskA }
    val tb = task { taskB }
    val tc = task { taskC }
    val td = taskD
    (ta.join(), tb.join(), tc.join(), td)
  }

}