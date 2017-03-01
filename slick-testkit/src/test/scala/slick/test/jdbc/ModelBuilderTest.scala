package slick.test.jdbc

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.Failure

import com.typesafe.slick.testkit.util.DBTest
import com.typesafe.slick.testkit.util.DBTestObject
import com.typesafe.slick.testkit.util.JdbcTestDB
import com.typesafe.slick.testkit.util.StandardTestDBs._
import org.junit.Test
import org.junit.Assert._
import slick.testutil._

object ModelBuilderTest extends DBTestObject(H2Mem)

class ModelBuilderTest(val tdb: JdbcTestDB) extends DBTest {
  import tdb.profile.api._

  @Test
  def test(): Unit = {
    // test timestamps don't fail
    val a =
      sqlu"""create table BAR (FOO TIMESTAMP DEFAULT CURRENT_TIMESTAMP)""" >>
      tdb.profile.createModel(ignoreInvalidDefaults=false) >>
      sqlu"""create table BAZ (FOO VARCHAR(255) DEFAULT CURRENT_USER)""" >>
      tdb.profile.createModel(ignoreInvalidDefaults=false).asTry
    val mt = Await.result(db.run(a.withPinnedSession), Duration.Inf)
    assertTrue(mt.asInstanceOf[Failure[_]].exception.asInstanceOf[SlickException].getMessage.contains("not parse default"))
  }
}
