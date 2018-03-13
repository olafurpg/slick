package slick.jdbc.meta

import slick.dbio.Effect
import slick.jdbc.ResultSetAction
import slick.basic.BasicStreamingAction
import scala.`package`.Vector
import slick.dbio.Effect.Read
import slick.jdbc.meta.{ MBestRowIdentifierColumn, MColumn, MForeignKey, MIndexInfo, MPrimaryKey, MTable, MTablePrivilege, MVersionColumn }

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getTables(). */
case class MTable(
  name: MQName, tableType: String, remarks: String, typeName: Option[MQName],
  selfRefColName: Option[String], refGen: Option[String]) {

  def getColumns: BasicStreamingAction[Vector[MColumn], MColumn, Read] = MColumn.getColumns(name, "%")
  def getPrimaryKeys: BasicStreamingAction[Vector[MPrimaryKey], MPrimaryKey, Read] = MPrimaryKey.getPrimaryKeys(name)
  def getImportedKeys: BasicStreamingAction[Vector[MForeignKey], MForeignKey, Read] = MForeignKey.getImportedKeys(name)
  def getExportedKeys: BasicStreamingAction[Vector[MForeignKey], MForeignKey, Read] = MForeignKey.getExportedKeys(name)
  def getVersionColumns: BasicStreamingAction[Vector[MVersionColumn], MVersionColumn, Read] = MVersionColumn.getVersionColumns(name)
  def getTablePrivileges: BasicStreamingAction[Vector[MTablePrivilege], MTablePrivilege, Read] = MTablePrivilege.getTablePrivileges(name)
  def getBestRowIdentifier(scope: MBestRowIdentifierColumn.Scope, nullable: Boolean = false): BasicStreamingAction[Vector[MBestRowIdentifierColumn], MBestRowIdentifierColumn, Read] =
    MBestRowIdentifierColumn.getBestRowIdentifier(name, scope, nullable)
  /** @param unique when true, return only indices for unique values; when false, return indices regardless of whether unique or not */
  def getIndexInfo(unique: Boolean = false, approximate: Boolean = false): BasicStreamingAction[Vector[MIndexInfo], MIndexInfo, Read] =
    MIndexInfo.getIndexInfo(name, unique, approximate)
}

object MTable {
  def getTables(cat: Option[String], schemaPattern: Option[String], namePattern: Option[String],
    types: Option[Seq[String]]): BasicStreamingAction[Vector[MTable], MTable, Read] = ResultSetAction[MTable](
      _.metaData.getTables(cat.orNull, schemaPattern.orNull, namePattern.orNull, types.map(_.toArray).orNull) ) { r =>
      if(r.numColumns > 5) MTable(MQName.from(r), r.<<, r.<<, MQName.optionalFrom(r), r.<<, r.<<)
      else MTable(MQName.from(r), r.<<, r.<<, None, None, None)
  }
  def getTables(namePattern: String): BasicStreamingAction[Vector[MTable], MTable, Effect.Read] = getTables(Some(""), Some(""), Some(namePattern), None)
  def getTables: BasicStreamingAction[Vector[MTable], MTable, Effect.Read] = getTables(Some(""), Some(""), None, None)
}
