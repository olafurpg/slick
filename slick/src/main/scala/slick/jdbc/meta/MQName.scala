package slick.jdbc.meta

import slick.jdbc.PositionedResult
import java.lang
import slick.jdbc.meta.MQName

/** A qualified name with an optional catalog and schema. */
case class MQName(catalog: Option[String], schema: Option[String], name: String) {
  override def toString: lang.String = "MQName(" + catalog.map(_ + ".").getOrElse("") + schema.map(_ + ".").getOrElse("") + name + ")"

  def catalog_? : String = catalog.orNull
  def schema_? : String = schema.orNull
}

object MQName {
  private[meta] def from(r: PositionedResult) = MQName(r.<<, r.<<, r.<<)

  private[meta] def optionalFrom(r: PositionedResult) = {
    val cat = r.nextStringOption
    val schema = r.nextStringOption
    r.nextStringOption map (MQName(cat, schema, _))
  }

  def local(name: String): MQName = MQName(Some(""), Some(""), name)
}
