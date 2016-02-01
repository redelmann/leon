import leon.annotation._
import leon.lang._
import leon.parallel._
import leon.collection._

object ParallelAlgorithms {

  def fib(x: BigInt): BigInt = {
    require(x >= 0)
    if (x == 0 || x == 1) 1
    else
      fib(x - 1) + fib(x - 2)
  }

  val cutoff = BigInt(35) // cutoff for switching from parallel to sequential

  def fibPar(x: BigInt): BigInt ={
    require(x >= 0)
    if(x <= cutoff) fib(x)
    else{
      val (y, z) = parallel(fibPar(x-1), fibPar(x-2))
      y + z
    }
  }

  /**
   * A buggy version in which precondition of recursive calls will not hold.
   */
  def fibParBuggy(x: BigInt): BigInt ={
    require(x >= 1)
    if(x == 1) 1
    else{
      val (y, z) = parallel(fibParBuggy(x-1), fibParBuggy(x-2))
      y + z
    }
  }

  /**
   * A tree where data is stored only in the leaves
   */
  sealed abstract class Tree[T]
  case class Node[T](l: Tree[T], r: Tree[T]) extends Tree[T]
  case class Leaf[T](t: T) extends Tree[T]

  def fibForkJoin(x: BigInt): BigInt = {
    require(x >= 0)

    def recFork(x: BigInt): Tree[ForkJoinTask[BigInt]] = {
      require(x >= 0)
      if(x <= cutoff) Leaf(task { fib(x) })
      else {
        Node(recFork(x-1), recFork(x-2))
      }
    }

    def recJoin(l: Tree[ForkJoinTask[BigInt]]): BigInt = {
      l match {
        case Node(l, r) => recJoin(l) + recJoin(r)
        case Leaf(t) => t.join()
      }
    }
    recJoin(recFork(x))
  }

  @ignore
  def main(args: Array[String]) = {
    val n = 39
    val start = System.currentTimeMillis()
    val res1 = fibForkJoin(n)
    val endPrev = System.currentTimeMillis()
    println(s"$n th fib number: $res1 fork-join time: ${(endPrev - start)/1000.0}s")
    val res2 = fibPar(n)
    val endTime = System.currentTimeMillis()
    println(s"$n th fib number: $res2 par time: ${(endTime - endPrev)/1000.0}s")
    val res3 = fib(n)
    println(s"$n th fib number: $res3 seq time: ${(System.currentTimeMillis() - endTime)/1000.0}s")
  }
}
