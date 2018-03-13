package slick.jdbc.meta

import java.sql._
import slick.jdbc.{ResultSetAction, JdbcTypesComponent}
import java.lang
import scala.`package`.Vector
import slick.basic.BasicStreamingAction
import slick.dbio.Effect.Read
import slick.jdbc.meta.MBestRowIdentifierColumn

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getBestRowIdentifier(). */
case class MBestRowIdentifierColumn(
  scope: MBestRowIdentifierColumn.Scope, column: String, sqlType: Int, typeName: String,
  columnSize: Option[Int], decimalDigits: Option[Short], pseudoColumn: Option[Boolean]) {

  def sqlTypeName: Option[lang.String] = JdbcTypesComponent.typeNames.get(sqlType)
}

object MBestRowIdentifierColumn {
  def getBestRowIdentifier(table: MQName, scope: Scope, nullable: Boolean = false): BasicStreamingAction[Vector[MBestRowIdentifierColumn], MBestRowIdentifierColumn, Read] =
    ResultSetAction[MBestRowIdentifierColumn](
      _.metaData.getBestRowIdentifier(table.catalog_?, table.schema_?, table.name, scope.value, nullable)) { r =>
      MBestRowIdentifierColumn(Scope(r.<<), r.<<, r.<<, r.<<, r.<<, r.skip.<<, r.nextShort match {
          case DatabaseMetaData.bestRowNotPseudo => Some(false)
          case DatabaseMetaData.bestRowPseudo => Some(true)
          case _ => None
        })
  }

  sealed abstract class Scope(val value: Int)

  object Scope {
    final case object Temporary extends Scope(DatabaseMetaData.bestRowTemporary)
    final case object Transaction extends Scope(DatabaseMetaData.bestRowTransaction)
    final case object Session extends Scope(DatabaseMetaData.bestRowSession)
    private[MBestRowIdentifierColumn] def apply(value: Short) = value match {
      case DatabaseMetaData.bestRowTemporary => Temporary
      case DatabaseMetaData.bestRowTransaction => Transaction
      case DatabaseMetaData.bestRowSession => Session
    }
  }
}
