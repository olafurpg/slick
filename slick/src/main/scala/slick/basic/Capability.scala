package slick.basic

/** Describes a feature that can be supported by a profile. */
import slick.basic.Capability
class Capability(name: String) {
  override def toString: String = name
}

object Capability {
  def apply(name: String): Capability = new Capability(name)
}
