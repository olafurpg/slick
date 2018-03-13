package slick.jdbc.meta

import slick.jdbc.{PositionedResult, ResultSetAction}
import slick.jdbc.GetResult.GetString
import scala.`package`.Vector
import slick.basic.BasicStreamingAction
import slick.dbio.Effect.Read

/**
 * Accessor methods for various database meta data.
 */
object DatabaseMeta {

  def getCatalogs: BasicStreamingAction[Vector[String], String, Read] = ResultSetAction[String](_.metaData.getCatalogs())

  def getTableTypes: BasicStreamingAction[Vector[String], String, Read] = ResultSetAction[String](_.metaData.getTableTypes())

  private[meta] def yesNoOpt(r: PositionedResult) = if(r.hasMoreColumns) r.nextString match {
    case "YES" => Some(true)
    case "NO" => Some(false)
    case _ => None
  } else None
}
