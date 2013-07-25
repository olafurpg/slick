package scala.slick.yy

import scala.language.implicitConversions
import scala.slick.driver.{ H2Driver, JdbcDriver }
import scala.slick.jdbc.{ JdbcBackend, UnitInvoker }

object Shallow {
  type Invoker[T] = (JdbcDriver => UnitInvoker[T])
  object Queryable {
    def apply[T]: Query[T] = ???
  }
  // TODO AnyVal is necessary or not
  implicit class SingleColumnQuery[T <: AnyVal](val query: Query[T]) extends AnyVal {
    def min: Option[T] = ???
    def max: Option[T] = ???
    def avg: Option[T] = ???
    def sum: Option[T] = ???
  }
  def nonesFirst[T]: Ordering[Option[T]] = ???
  def nonesLast[T]: Ordering[Option[T]] = ???
  def nullsFirst[T]: Ordering[T] = ???
  def nullsLast[T]: Ordering[T] = ???
  trait Query[T] {
    def flatMap[S](projection: T => Query[S]): Query[S] = ???
    def map[S](projection: T => S): Query[S] = ???
    def filter(projection: T => Boolean): Query[T] = ???
    def withFilter(projection: T => Boolean): Query[T] = ???
    def length: Int = ???
    def sortBy[S](projection: T => S)(implicit ord: Ordering[S]): Query[T] = ???
    def sorted(implicit ord: Ordering[T]): Query[T] = ???
    def groupBy[S](f: T => S): Query[(S, Shallow.Query[T])] = ???
    def innerJoin[S](q2: Query[S]): JoinQuery[T, S] = ???
    def leftJoin[S](q2: Query[S]): JoinQuery[T, S] = ???
    def rightJoin[S](q2: Query[S]): JoinQuery[T, S] = ???
    def outerJoin[S](q2: Query[S]): JoinQuery[T, S] = ???
    def zip[S](q2: Query[S]): JoinQuery[T, S] = ???
    def zipWithIndex: JoinQuery[T, Long] = ???
    def take(i: Int): Query[T] = ???
    def drop(i: Int): Query[T] = ???
    //    def toSeq: Seq[T] = ???
    //    def first: T = ???
    //    def toSeqTemplate: Seq[T] = ???
    //    def firstTemplate: T = ???
    //    def getQueryTemplate[P]: QueryTemplate[P, T] = ???
    //    def funcTemplate: QueryTemplate[Any, T] = ???
    //    def getInvoker: Invoker[T] = ???
    //    def firstImplicit: (JdbcDriver => JdbcBackend#Session => T) = ???
    //    def toSeqImplicit: (JdbcDriver => JdbcBackend#Session => Seq[T]) = ???
    //    def insert(value: T): Int = ???
    //    def update(value: T): Int = ???
    //    def executor: Executor[T] = ???
  }
  //  type Executor[T] = QueryExecutor[T]
  trait JoinQuery[T1, T2] extends Query[(T1, T2)] {
    def on(pred: (T1, T2) => Boolean): Query[(T1, T2)] = ???
  }
  implicit def stringWrapper(value: String): ColumnOps[String] = new ColumnOps(value)
  implicit def intWrapper(value: Int): ColumnOps[Int] = new ColumnOps(value)
  // FIXME operations for Int, Float, String, Double, and Boolean should be separated
  implicit class ColumnOps[T](val value: T) extends AnyVal {
    def abs: T = ???
    def ceil: T = ???
    def floor: T = ???
    def sign: T = ???
    def toDegrees: T = ???
    def toRadians: T = ???
    def ++(o: String): String = ???
    def like(o: String): Boolean = ???
    def ltrim: String = ???
    def rtrim: String = ???
  }
  object Query {
    def ofTable[T](i: Table[T]): Query[T] = ???
    def apply[T](i: T): Query[T] = ???
  }
  class Table[T]
  object Table {
    def getTable[S]: Table[S] = ???
  }
  implicit class OptMaker[T](val value: T) {
    def ? : Option[T] = ???
  }
  class QueryTemplate[-P, R] {
    def apply(param: P): Seq[R] = ???
  }
  implicit def queryToShadowExecutor[T](query: Query[T]): ShadowExecutor[T] = new ShadowExecutor(query)
  object TestH2 {
    val driver = H2Driver
    implicit val h2Driver = driver
    implicit def h2Session = _session
    private val conn = h2Driver.simple.Database.forURL("jdbc:h2:mem:test14", driver = "org.h2.Driver")
    private var _session = conn.createSession
    def provideSession: JdbcBackend#Session = _session
    def closeSession {
      _session.close
      _session = conn.createSession
    }
  }
}
