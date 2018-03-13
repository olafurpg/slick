package slick.jdbc

import slick.basic.Capability
import scala.collection.immutable

/** Capabilities for [[slick.jdbc.JdbcProfile]]. */
object JdbcCapabilities {
  /** Can be used for reverse-engineering the database schema */
  val createModel: Capability = Capability("jdbc.createModel")
  /** Can insert into AutoInc columns. */
  val forceInsert: Capability = Capability("jdbc.forceInsert")
  /** Supports a native insertOrUpdate command. Otherwise the functionality
    * is emulated on the client side. The emulation uses transactions for
    * consistency but does not guarantee atomicity, so it may fail if another
    * insert for the same key happens concurrently. */
  val insertOrUpdate: Capability = Capability("jdbc.insertOrUpdate")
  /** Supports mutable result sets */
  val mutable: Capability = Capability("jdbc.mutable")
  /** Can return primary key of inserted rows */
  val returnInsertKey: Capability = Capability("jdbc.returnInsertKey")
  /** Can also return non-primary-key columns of inserted rows */
  val returnInsertOther: Capability = Capability("jdbc.returnInsertOther")
  /** Returns column default values in meta data */
  val defaultValueMetaData: Capability = Capability("jdbc.defaultValueMetaData")
  /** Doesn't map types to Boolean in DatabaseMetaData */
  val booleanMetaData: Capability = Capability("jdbc.booleanMetaData")
  /** Reports no default and NULL default differently in meta data */
  val nullableNoDefault: Capability = Capability("jdbc.nullableNoDefault")
  /** Makes a difference between different integer types */
  val distinguishesIntTypes: Capability = Capability("jdbc.distinguishesIntTypes")
  /** Has a datatype directly corresponding to Scala Byte */
  val supportsByte: Capability = Capability("jdbc.supportsByte")
  /** Supports FOR UPDATE row level locking */
  val forUpdate: Capability = Capability("jdbc.forUpdate")

  /** Supports all JdbcProfile features which do not have separate capability values */
  val other: Capability = Capability("jdbc.other")

  /** All JDBC capabilities */
  val all: immutable.Set[Capability] = Set(other, createModel, forceInsert, insertOrUpdate, mutable, returnInsertKey, defaultValueMetaData, booleanMetaData, nullableNoDefault, distinguishesIntTypes, supportsByte, returnInsertOther, forUpdate)
}
