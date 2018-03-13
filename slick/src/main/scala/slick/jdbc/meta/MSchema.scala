package slick.jdbc.meta

import slick.jdbc.ResultSetAction
import java.lang
import scala.`package`.Vector
import slick.basic.BasicStreamingAction
import slick.dbio.Effect.Read
import slick.jdbc.meta.MSchema

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getSchemas(). */
case class MSchema(schema: String, catalog: Option[String]) {
  override def toString: lang.String = "MSchema(" + catalog.map(_ + ".").getOrElse("") + schema + ")"
}

object MSchema {
  def getSchemas(catalog: Option[String], schemaPattern: Option[String]): BasicStreamingAction[Vector[MSchema], MSchema, Read] = {
    ResultSetAction[MSchema] { s =>
      try s.metaData.getSchemas(catalog.orNull, schemaPattern.orNull)
      catch { case _: AbstractMethodError => null }
    } { r => MSchema(r.<<, r.<<?) }
  }

  def getSchemas: BasicStreamingAction[Vector[MSchema], MSchema, Read] = ResultSetAction[MSchema](_.metaData.getSchemas()) { r => MSchema(r.<<, r.<<?) }
}
