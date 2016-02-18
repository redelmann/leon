/* Copyright 2009-2015 EPFL, Lausanne */

package leon

import leon.utils._

import java.io.File

import scala.reflect.ClassTag

/** Everything that is part of a compilation unit, except the actual program tree.
  * LeonContexts are immutable, and so should all their fields (with the possible
  * exception of the reporter).
  */
class LeonContext(
  val reporter: Reporter,
  val interruptManager: InterruptManager,
  val options: Seq[LeonOption[Any]] = Seq(),
  val files: Seq[File] = Seq(),
  val classDir: Option[File] = None,
  val timers: TimerStorage = new TimerStorage
) {

  def findOption[A: ClassTag](optDef: LeonOptionDef[A]): Option[A] = options.collectFirst {
    case LeonOption(`optDef`, value:A) => value
  }

  def findOptionOrDefault[A: ClassTag](optDef: LeonOptionDef[A]): A =
    findOption(optDef).getOrElse(optDef.default)

  def copy(
    reporter: Reporter = this.reporter,
    interruptManager: InterruptManager = this.interruptManager,
    options: Seq[LeonOption[Any]] = this.options,
    files: Seq[File] = this.files,
    classDir: Option[File] = this.classDir,
    timers: TimerStorage = this.timers
  ) = {
    new LeonContext(
      reporter,
      interruptManager,
      options,
      files,
      classDir,
      timers
    )
  }

}

object LeonContext {
  def empty = {
    val reporter = new DefaultReporter(Set())
    new LeonContext(reporter, new InterruptManager(reporter))
  }

  def printNames = {
    val reporter = new DefaultReporter(Set())
    new LeonContext(
      reporter,
      new InterruptManager(reporter),
      options = Seq(LeonOption[Set[DebugSection]](SharedOptions.optDebug)(Set(DebugSectionTrees)))
    )
  }
}
