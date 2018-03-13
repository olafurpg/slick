package slick.util

import java.io.Closeable
import scala.util.control.NonFatal
import slick.util.CloseableIterator
import slick.util.CloseableIterator.Close

/**
 * An Iterator with a `close` method to close the underlying data source.
 * Implementers must close the data source when `hasNext` returns `false`.
 */
trait CloseableIterator[+T] extends Iterator[T] with Closeable { self =>

  /**
   * Close the underlying data source. The behaviour of any methods of this
   * object after closing it is undefined.
   */
  override def close(): Unit

  override def map[B](f: T => B): CloseableIterator[B] = new CloseableIterator[B] {
    def hasNext: Boolean = self.hasNext
    def next(): B = f(self.next())
    def close(): Unit = self.close()
  }

  final def use[R](f: (Iterator[T] => R)): R =
    try f(this) finally close()

  final def use[R](f: =>R): R =
    try f finally close()

  /**
   * Return a new CloseableIterator which also closes the supplied Closeable
   * object when itself gets closed.
   */
  final def thenClose(c: Closeable): CloseableIterator[T] = new CloseableIterator[T] {
    def hasNext: Boolean = self.hasNext
    def next(): T = self.next()
    def close(): Unit = try self.close() finally c.close()
  }

  protected final def noNext: Nothing = throw new NoSuchElementException("next on empty iterator")
}

object CloseableIterator {

  /**
   * An empty CloseableIterator
   */
  val empty: CloseableIterator[Nothing] = new CloseableIterator[Nothing] {
    def hasNext = false
    def next(): Nothing = noNext
    def close() {}
  }

  /**
   * A CloseableIterator which contains exactly one item.
   */
  class Single[+T](item: T) extends CloseableIterator[T] {
    private var more = true
    def hasNext: Boolean = more
    def next(): T = if(more) { more = false; item } else noNext
    def close {}
  }

  /**
   * Using some Closeable resource and a function to create a CloseableIterator
   * from it, return a wrapped CloseableIterator which closes the resource when
   * itself gets closed. If the function terminates abnormally, the resource is
   * closed immediately.
   */
  def close[C <: Closeable](makeC: => C): Close[C] = new Close[C](makeC)

  final class Close[C <: Closeable](makeC: => C) {
    def after[T](f: C => CloseableIterator[T]): CloseableIterator[T] = {
      val c = makeC
      (try f(c) catch { case NonFatal(e) =>
        try c.close() catch ignoreFollowOnError
        throw e
      }) thenClose c
    }
  }
}
