package slick.jdbc.meta

import slick.jdbc.ResultSetAction
import scala.`package`.Vector
import slick.basic.BasicStreamingAction
import slick.dbio.Effect.Read
import slick.jdbc.meta.MSuperTable

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getSuperTables(). */
case class MSuperTable(table: MQName, superTable: String) {
  def getSuperTables: BasicStreamingAction[Vector[MSuperTable], MSuperTable, Read] = MSuperTable.getSuperTables(MQName(table.catalog, table.schema, superTable))
}

object MSuperTable {
  def getSuperTables(tablePattern: MQName): BasicStreamingAction[Vector[MSuperTable], MSuperTable, Read] = ResultSetAction[MSuperTable](
      _.metaData.getSuperTables(tablePattern.catalog_?, tablePattern.schema_?, tablePattern.name) ) { r =>
      MSuperTable(MQName.from(r), r.<<)
  }
}
