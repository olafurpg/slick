package slick.jdbc

import java.sql.ResultSet
import slick.dbio.Effect
import slick.basic.BasicStreamingAction
import slick.util.CloseableIterator
import scala.collection.immutable
import scala.collection.mutable.Builder
import slick.jdbc.Invoker

/** An invoker which calls a function to retrieve a ResultSet. This can be used
  * for reading information from a java.sql.DatabaseMetaData object which has
  * many methods that return ResultSets.
  *
  * For convenience, if the function returns null, this is treated like an
  * empty ResultSet. */
abstract class ResultSetInvoker[+R] extends Invoker[R] { self =>

  protected def createResultSet(session: JdbcBackend#Session): ResultSet

  def iteratorTo(maxRows: Int)(implicit session: JdbcBackend#Session): CloseableIterator[R] = {
    val rs = createResultSet(session)
    if(rs eq null) CloseableIterator.empty
    else {
      val pr = new PositionedResult(rs) {
        def close(): Unit = rs.close()
      }
      new PositionedResultIterator[R](pr, maxRows, true) {
        def extractValue(pr: PositionedResult): R = self.extractValue(pr)
      }
    }
  }

  protected def extractValue(pr: PositionedResult): R
}

object ResultSetInvoker {
  def apply[R](f: JdbcBackend#Session => ResultSet)(implicit conv: PositionedResult => R): Invoker[R] = new ResultSetInvoker[R] {
    def createResultSet(session: JdbcBackend#Session): ResultSet = f(session)
    def extractValue(pr: PositionedResult): R = conv(pr)
  }
}

object ResultSetAction {
  def apply[R](f: JdbcBackend#Session => ResultSet)(implicit conv: PositionedResult => R): BasicStreamingAction[Vector[R], R, Effect.Read] = new StreamingInvokerAction[Vector[R], R, Effect.Read] {
    protected[this] def createInvoker(sql: Iterable[String]): Invoker[R] = ResultSetInvoker(f)(conv)
    protected[this] def createBuilder: Builder[R, immutable.Vector[R]] = Vector.newBuilder[R]
    def statements: immutable.Nil.type = Nil
  }
}
