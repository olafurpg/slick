package slick.relational

import slick.basic.Capability
import scala.collection.immutable

/** Capabilities for [[slick.profile.RelationalProfile]]. */
object RelationalCapabilities {
  /** Supports default values in column definitions */
  val columnDefaults: Capability = Capability("relational.columnDefaults")
  /** Supports foreignKeyActions */
  val foreignKeyActions: Capability = Capability("relational.foreignKeyActions")
  /** Supports the ''database'' function to get the current database name.
    * A profile without this capability will return an empty string. */
  val functionDatabase: Capability = Capability("relational.functionDatabase")
  /** Supports the ''user'' function to get the current database user.
    * A profile without this capability will return an empty string. */
  val functionUser: Capability = Capability("relational.functionUser")
  /** Supports indexOf method on string columns */
  val indexOf: Capability = Capability("relational.indexOf")
  /** Supports repeat method on string columns */
  val repeat: Capability = Capability("relational.repeat")
  /** Supports full outer joins */
  val joinFull: Capability = Capability("relational.joinFull")
  /** Supports left outer joins */
  val joinLeft: Capability = Capability("relational.joinLeft")
  /** Supports right outer joins */
  val joinRight: Capability = Capability("relational.joinRight")
  /** Supports escape characters in "like" */
  val likeEscape: Capability = Capability("relational.likeEscape")
  /** Supports .drop on queries */
  val pagingDrop: Capability = Capability("relational.pagingDrop")
  /** Supports properly compositional paging in sub-queries */
  val pagingNested: Capability = Capability("relational.pagingNested")
  /** Returns only the requested number of rows even if some rows are not
    * unique. Without this capability, non-unique rows may be counted as
    * only one row each. */
  val pagingPreciseTake: Capability = Capability("relational.pagingPreciseTake")
  /** Supports replace method on string columns */
  val replace: Capability = Capability("relational.replace")
  /** Supports reverse method on string columns */
  val reverse: Capability = Capability("relational.reverse")
  /** Can set an Option[ Array[Byte] ] column to None */
  val setByteArrayNull: Capability = Capability("relational.setByteArrayNull")
  /** Supports the BigDecimal data type */
  val typeBigDecimal: Capability = Capability("relational.typeBigDecimal")
  /** Supports the Blob data type */
  val typeBlob: Capability = Capability("relational.typeBlob")
  /** Supports the Long data type */
  val typeLong: Capability = Capability("relational.typeLong")
  /** Supports zip, zipWith and zipWithIndex */
  val zip: Capability = Capability("relational.zip")

  /** Supports all RelationalProfile features which do not have separate capability values */
  val other: Capability = Capability("relational.other")

  /** All relational capabilities */
  val all: immutable.Set[Capability] = Set(other, columnDefaults, foreignKeyActions, functionDatabase,
    functionUser, joinFull, joinLeft, joinRight, likeEscape, pagingDrop, pagingNested,
    pagingPreciseTake, setByteArrayNull, typeBigDecimal, typeBlob, typeLong,
    zip, replace, reverse, indexOf, repeat)
}
