package slick.lifted

import java.sql.{Date, Time}
import slick.ast.{TypedType, Library}
import FunctionSymbolExtensionMethods._
import slick.ast.ScalaBaseType._
import slick.lifted.Rep

/** Contains stand-alone database functions for use in queries. Functions which
  * operate on columns are generally added as extension methods to the appropriate
  * column types instead. */
object Functions {

  /** The name of the database user, or an empty string if not supported by the DBMS */
  val user: Rep[String] = Library.User.column[String]()

  /** The name of the database, or an empty string if not supported by the DBMS */
  val database: Rep[String] = Library.Database.column[String]()

  /** The current date of the database server */
  def currentDate(implicit tpe: TypedType[Date]): Rep[Date] = Library.CurrentDate.column[Date]()

  /** The current time of the database server */
  def currentTime(implicit tpe: TypedType[Time]): Rep[Time] = Library.CurrentTime.column[Time]()

  /** The numeric constant for pi */
  def pi(implicit tpe: TypedType[Double]): Rep[Double] = Library.Pi.column[Double]()
}
