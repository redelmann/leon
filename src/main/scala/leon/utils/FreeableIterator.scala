/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package utils

abstract class FreeableIterator[T] extends Iterator[T] {
  private[this] var nextElem: Option[T] = None

  def hasNext = {
    nextElem = computeNext()
    nextElem.nonEmpty
  }

  def next() = {
    nextElem.get
  }

  def computeNext(): Option[T]

  def free()

  override def map[B](f: T => B): FreeableIterator[B] = {
    val orig = this

    new FreeableIterator[B] {
      def computeNext() = orig.computeNext.map(f)
      def free() = orig.free()
    }
  }

  override def take(n: Int): FreeableIterator[T] = {
    val orig = this

    new FreeableIterator[T] {
      private var c = 0;

      def computeNext() = {
        if (c < n) {
          c += 1
          orig.computeNext
        } else {
          None
        }
      }

      def free() = orig.free()
    }
  }

  override def toList: List[T] = {
    val res = super.toList
    free()
    res
  }
}

object FreeableIterator {
  def empty[T] = new FreeableIterator[T] {
    def computeNext() = None
    def free() = {}
  }
}
