package slick.jdbc.meta

import java.sql._
import slick.jdbc.ResultSetAction
import scala.`package`.Vector
import slick.basic.BasicStreamingAction
import slick.dbio.Effect.Read
import slick.jdbc.meta.{ MProcedure, MProcedureColumn }

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getProcedures(). */
case class MProcedure(name: MQName, remarks: String, returnsResult: Option[Boolean], specificName: Option[String]) {
  def getProcedureColumns(columnNamePattern: String = "%"): BasicStreamingAction[Vector[MProcedureColumn], MProcedureColumn, Read] =
    MProcedureColumn.getProcedureColumns(name, columnNamePattern)
}

object MProcedure {
  def getProcedures(namePattern: MQName): BasicStreamingAction[Vector[MProcedure], MProcedure, Read] = ResultSetAction[MProcedure](
      _.metaData.getProcedures(namePattern.catalog_?, namePattern.schema_?, namePattern.name) ) { r =>
      MProcedure(MQName.from(r), r.skip.skip.skip.<<, r.nextShort match {
          case DatabaseMetaData.procedureNoResult => Some(false)
          case DatabaseMetaData.procedureReturnsResult => Some(true)
          case _ => None
        }, r.<<?)
  }
}
