package slick.jdbc.meta

import java.sql._
import slick.jdbc.{ResultSetAction, JdbcTypesComponent}
import java.lang
import scala.`package`.Vector
import slick.basic.BasicStreamingAction
import slick.dbio.Effect.Read
import slick.jdbc.meta.MProcedureColumn

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getProcedureColumns(). */
case class MProcedureColumn(
  procedure: MQName, column: String, columnType: Short, sqlType: Int, typeName: String,
  precision: Option[Int], length: Int, scale: Option[Short], radix: Short,
  nullable: Option[Boolean], remarks: String, columnDef: Option[String], charOctetLength: Option[Int],
  ordinalPosition: Option[Int], isNullable: Option[Boolean], specificName: Option[String]) {

  def sqlTypeName: Option[lang.String] = JdbcTypesComponent.typeNames.get(sqlType)
}

object MProcedureColumn {
  def getProcedureColumns(procedurePattern: MQName, columnNamePattern: String = "%"): BasicStreamingAction[Vector[MProcedureColumn], MProcedureColumn, Read] = ResultSetAction[MProcedureColumn](
      _.metaData.getProcedureColumns(procedurePattern.catalog_?, procedurePattern.schema_?,
                                     procedurePattern.name, columnNamePattern) ) { r =>
      MProcedureColumn(MQName.from(r), r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.nextShort match {
          case DatabaseMetaData.procedureNoNulls => Some(false)
          case DatabaseMetaData.procedureNullable => Some(true)
          case _ => None
        }, r.<<, r.<<?, r.skip.skip.<<?, r.<<?, DatabaseMeta.yesNoOpt(r), r.<<?)
  }
}
