package slick.jdbc.meta

import java.sql._

import slick.jdbc.JdbcTypesComponent
import slick.jdbc.ResultSetAction

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getColumns(). */
case class MColumn(
  table: MQName, name: String, sqlType: Int, typeName: String,
  size: Option[Int], decimalDigits: Option[Int], numPrecRadix: Int, nullable: Option[Boolean], remarks: Option[String],
  columnDef: Option[String], charOctetLength: Int, ordinalPosition: Int, isNullable: Option[Boolean], scope: Option[MQName],
  sourceDataType: Option[Any], isAutoInc: Option[Boolean]) {

  def sqlTypeName = JdbcTypesComponent.typeNames.get(sqlType)
  def getColumnPrivileges = MColumnPrivilege.getColumnPrivileges(table, name)
}

object MColumn {
  def getColumns(tablePattern: MQName, columnPattern: String) = ResultSetAction[MColumn](
      _.metaData.getColumns(tablePattern.catalog_?, tablePattern.schema_?, tablePattern.name, columnPattern)) { r =>
      MColumn(MQName.from(r), r.<<, r.<<, r.<<, r.<<, r.skip.<<, r.<<, r.nextInt match {
          case DatabaseMetaData.columnNoNulls => Some(false)
          case DatabaseMetaData.columnNullable => Some(true)
          case _ => None
        }, r.<<, r.<<, r.skip.skip.<<, r.<<, DatabaseMeta.yesNoOpt(r),
        if(r.hasMoreColumns) MQName.optionalFrom(r) else None,
        r.nextObjectOption(),
        if(r.hasMoreColumns) DatabaseMeta.yesNoOpt(r) else None)
  }
}
