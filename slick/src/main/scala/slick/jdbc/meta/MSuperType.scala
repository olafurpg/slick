package slick.jdbc.meta

import slick.jdbc.ResultSetAction
import scala.`package`.Vector
import slick.basic.BasicStreamingAction
import slick.dbio.Effect.Read
import slick.jdbc.meta.MSuperType

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getSuperTypes(). */
case class MSuperType(typeName: MQName, superType: MQName) {
  def getSuperTypes: BasicStreamingAction[Vector[MSuperType], MSuperType, Read] = MSuperType.getSuperTypes(superType)
}

object MSuperType {
  def getSuperTypes(typePattern: MQName): BasicStreamingAction[Vector[MSuperType], MSuperType, Read] = ResultSetAction[MSuperType](
      _.metaData.getSuperTypes(typePattern.catalog_?, typePattern.schema_?, typePattern.name) ) { r =>
      MSuperType(MQName.from(r), MQName.from(r))
  }
}
