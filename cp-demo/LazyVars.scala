import cp.Terms._
import cp.Definitions._
import cp.LTrees._

object LazyVars {
  def NonnegativeInt = ((x: Int) => x >= 0).lazyFindAll
  def chooseInt(lower: Int, upper: Int) = ((x: Int) => x >= lower && x <= upper).lazyFindAll

  def main(args: Array[String]): Unit = {
    f1()
    println
    f2()
    println
    f3()
    println
  }

  def f1() {
    for {
      x <- chooseInt(0, 5)
      y <- chooseInt(1, 3)
      if x > y
    } {
      val a: Int = x
      val b: Int = y
      println(a, b)
    }
  }

  def f2() {
    // will loop forever if Scala Stream.from(0) is used instead.
    for {
      x <- NonnegativeInt
      if x < 3
      y <- NonnegativeInt
      if x > y
    } {
      val i: Int = x
      val j: Int = y
      println(i, j)
    }
  }

  def f3() {
    for {
      i <- chooseInt(0, 3)
      pair <- ((x: Int, y: Int) => x > 0 && y == 2 * x && x <= i).lazyFindAll
    } {
      println("i, (a, b):" + i.value + ", " + pair.value)
    }
  }

  def f4() {
    val p = ((x: Int, y: Int) => x > 0 && y == 2 * x && x <= 5).lazySolve
    val other = ((z: Int) => z == p._1 + p._2).lazySolve
    println("x + y: " + other.value)
    val x = p._1
    val y = p._2
    println("x: " + x.value)
    println("y: " + y.value)
  }
}
