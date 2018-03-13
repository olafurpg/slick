package slick.jdbc.meta

import slick.jdbc.{ResultSetAction, JdbcTypesComponent}
import java.lang
import scala.`package`.Vector
import slick.basic.BasicStreamingAction
import slick.dbio.Effect.Read
import slick.jdbc.meta.{ MAttribute, MUDT }

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getUDTs(). */
case class MUDT(
  typeName: MQName, className: String, sqlType: Int, remarks: String, baseType: Option[Short]) {

  def sqlTypeName: Option[lang.String] = JdbcTypesComponent.typeNames.get(sqlType)
  def getAttributes(attributeNamePattern: String = "%"): BasicStreamingAction[Vector[MAttribute], MAttribute, Read] =
    MAttribute.getAttributes(typeName, attributeNamePattern)
}

object MUDT {
  def getUDTs(typeNamePattern: MQName, types: Option[Seq[Int]] = None): BasicStreamingAction[Vector[MUDT], MUDT, Read] = ResultSetAction[MUDT](
      _.metaData.getUDTs(typeNamePattern.catalog_?, typeNamePattern.schema_?,
                         typeNamePattern.name, types.map(_.toArray)getOrElse(null))) { r =>
      MUDT(MQName.from(r), r.<<, r.<<, r.<<, r.<<)
  }
}
