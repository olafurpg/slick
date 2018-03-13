package slick.jdbc.meta

import slick.jdbc.ResultSetAction
import scala.`package`.Vector
import slick.basic.BasicStreamingAction
import slick.dbio.Effect.Read
import slick.jdbc.meta.MClientInfoProperty

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getClientInfoProperties(). */
case class MClientInfoProperty(name: String, maxLen: Int, defaultValue: String, description: String)

object MClientInfoProperty {
  def getClientInfoProperties: BasicStreamingAction[Vector[MClientInfoProperty], MClientInfoProperty, Read] = {
    ResultSetAction[MClientInfoProperty]{ s =>
      try s.metaData.getClientInfoProperties()
      catch { case _: AbstractMethodError => null }
    } { r =>
      MClientInfoProperty(r.<<, r.<<, r.<<, r.<<)
    }
  }
}
