package slick.memory

import slick.basic.Capability
import scala.collection.immutable

/** Capabilities for [[MemoryProfile]]. */
object MemoryCapabilities {
  /** Supports all MemoryProfile features which do not have separate capability values */
  val other: Capability = Capability("memory.other")

  /** All MemoryProfile capabilities */
  val all: immutable.Set[Capability] = Set(other)
}
