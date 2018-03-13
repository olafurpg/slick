package slick.util

/** A wrapper for a value, which uses reference equality of the wrapped
  * value as its own equality. This can be used, for example, to get the
  * equivalent of an `IdentityHashMap` from a regular `HashMap`. */
import java.lang
final case class RefId[E <: AnyRef](e: E) {
  override def hashCode: Int = System.identityHashCode(e)
  override def equals(o: Any): Boolean = o match {
    case RefId(e2) => e eq e2
    case _ => false
  }
  override def toString: lang.String = "RefId("+e.toString+")@" + hashCode
  def apply(): E = e
}
