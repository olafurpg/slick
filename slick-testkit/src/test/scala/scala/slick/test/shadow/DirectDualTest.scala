package scala.slick.test.shadow

import scala.language.{ reflectiveCalls, implicitConversions }
import org.junit.Test
import org.junit.Assert._
import scala.slick.testutil._
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag
import com.typesafe.slick.testkit.util.TestDB
import slick.jdbc.StaticQuery.interpolation
import scala.slick.SlickException
import scala.slick.shadow._
import ch.qos.logback.core.pattern.util.AsIsEscapeUtil
import scala.slick.shadow.test.YYDefinitions._

object DirectDualTest extends DBTestObject(TestDBs.H2Mem, TestDBs.H2Disk, TestDBs.HsqldbMem, TestDBs.HsqldbDisk, TestDBs.SQLiteMem, TestDBs.SQLiteDisk /*, TestDBs.DerbyMem, TestDBs.DerbyDisk*/ , TestDBs.Postgres)

class DirectDualTest(val tdb: TestDB) extends DBTest {
  implicit val testDriver = tdb.driver
  implicit val testSession = tdb.createDB().createSession

  object Singleton {
    import Shallow._
    // TODO needs some fixes in YY for ScopeInjection to be able to use composibility
    val q = stage {
      Queryable[Coffee]
    }.list()
    val q1 = stage {
      Queryable[Coffee].map(_.sales + 5)
    }.list()
    object Singleton {
      val q = stage {
        Queryable[Coffee]
      }.list()
      val q1 = stage {
        Queryable[Coffee].map(_.sales + 5)
      }.list()
      object Singleton {
        val q = stage {
          Queryable[Coffee]
        }.list()
        val q1 = stage {
          Queryable[Coffee].map(_.sales + 5)
        }.list()
      }
    }
  }

  object TestingTools {
    def assertMatchOrdered[T <: Product, T1 <: Product](expected: Vector[T], actual: Seq[T1]) =
      assertMatchCaseClass(expected, actual)
    def assertMatchCaseClass[T <: Product, T1 <: Product](expected: Vector[T], actual: Seq[T1]) = {
      assertEquals(expected.toList, actual.toList)
    }
    def assertMatch[T](expected: Vector[T], actual: Seq[T]) = {
      assertEquals(expected, actual.toVector)
    }
    def assertNoMatch[T](expected: Vector[T], actual: Seq[T]) = {
      assertNotSame(expected, actual.toVector)
    }

  }

  object SingletonInClass {
    import Shallow._
    val q1 = stage {
      Queryable[Coffee].map(_.sales + 5)
    }.list()
  }

  def initialStringOptionOrdering = implicitly[Ordering[Option[String]]]

  @Test def test() {
    implicit var stringOptionOrdering: scala.math.Ordering[Option[String]] = initialStringOptionOrdering

    import TestingTools._

    val coffees_data = Vector(
      ("Colombian", 1, None),
      ("French_Roast", 2, None),
      ("Espresso", 3, Some("Honey")),
      ("Espresso", 3, Some("Vanilla")),
      ("Espresso", 4, None),
      ("Colombian_Decaf", 1, None),
      ("Colombian_Decaf", 3, Some("White Chocolate")),
      ("French_Roast_Decaf", 5, None)
    )

    {
      // create test table
      sqlu"create table COFFEES(COF_NAME varchar(255), SALES int, FLAVOR varchar(255) NULL)".execute
      (for {
        (name, sales, flavor) <- coffees_data
      } yield sqlu"insert into COFFEES values ($name, $sales, $flavor)".first).sum

      // FIXME: reflective typecheck failed:  backend.result(Queryable[Coffee].map(x=>x))

      // setup query and equivalent inMemory data structure
      val inMem = coffees_data.map { case (name, sales, flavor) => Coffee(name, sales, flavor) }
      import Shallow._
      val query = stage {
        Queryable[Coffee]
      }

      // test framework sanity checks
      assertNoMatch(inMem ++ inMem, stage { query}.list())
      assertNoMatch(Vector(), stage { query}.list())

      // fetch whole table
      assertMatchCaseClass(inMem, stage { query}.list())
      // FIXME: make this test less artificial
      class MyQuerycollection {
        def findUserByName(name: String) = stage {
          query.filter(_.name == name)
        }.list()
      }
      val qc = new MyQuerycollection
      qc.findUserByName("some value")

      // test singleton object
      assertMatch(
        inMem.map(_.sales + 5),
        Singleton.q1)

      // test singleton in package
      assertMatch(
        inMem.map(_.sales + 5),
        Singleton.q1)

      // test singleton in singleton in package
      assertMatch(
        inMem.map(_.sales + 5),
        Singleton.Singleton.q1)

      // test singleton in singleton in singleton in package
      assertMatch(
        inMem.map(_.sales + 5),
        Singleton.Singleton.Singleton.q1)

      // test singleton in class (supported (!))
      assertMatch(inMem.map(_.sales + 5),
        SingletonInClass.q1
      )

      // simple map
      assertMatch(
        inMem.map((_: Coffee).sales + 5),
        stage {
          query.map(_.sales + 5)
        }.list())

      /*
      // left-hand-side coming from attribute
      val foo = new Foo(query)
      assertMatch(
        foo.q.map((_: Coffee).sales + 5),
        inMem.map((_: Coffee).sales + 5))
      */

      // map with string concatenation
      assertMatch(
        inMem.map(_.name + "."),
        stage {
          query.map(_.name + ".")
        }.list())

      // filter with more complex condition
      assertMatchCaseClass(
        inMem.filter(c => c.sales > 5 || "Chris" == c.name),
        stage {
          query.filter(c => c.sales > 5 || "Chris" == c.name)
        }.list())

      // type annotations FIXME canBuildFrom
      assertMatch(
        inMem.map((_: Coffee).name: String),
        stage {
          query.map[String](_.name: String)
        }.list())

      // chaining
      assertMatch(
        inMem.map(_.name).filter(_ == ""),
        stage {
          query.map(_.name).filter(_ == "")
        }.list())

      // referenced values are inlined as constants using reflection
      val o = 2 + 3
      assertMatchCaseClass(
        inMem.filter(_.sales > o),
        stage {
          query.filter(_.sales > o)
        }.list())

      // nesting (not supported yet: query.map(e1 => query.map(e2=>e1))) 
      assertMatchCaseClass(
        inMem.flatMap(e1 => inMem.map(e2 => e1)),
        stage {
          query.flatMap(e1 => query.map(e2 => e1))
        }.list())

      // query scope
      {
        val inMemResult = inMem.filter(_.sales > 5)
        List(
          stage {
            query.filter(_.sales > 5)
            }.list(),
          // Queryable( query.filter( _.sales > 5 ) ),
          stage {
            val foo = query
            val bar = foo.filter(_.sales > 5)
            bar
            }.list()).foreach {
            query_ => assertMatchCaseClass(inMemResult, query_)
          }
      }

      // comprehension with map
      assertMatch(
        for (c <- inMem) yield c.name,
        stage {
          (for (c <- query) yield c.name)
        }.list())

      // nesting with flatMap
      {
        val inMemResult = for (o <- inMem; i <- inMem) yield i.name
        List(
          stage {
            query.flatMap(o => query.map(i => i.name))
            }.list(),
          // Queryable(for (o <- query; i <- query) yield i.name), 
          stage {
            (for (o <- query; i <- query) yield i.name)
            }.list()).foreach {
            query_ => assertMatch(inMemResult, query_)
          }
      }

      assertMatchCaseClass(
        inMem.flatMap(e1 => inMem.map(e2 => e1).map(e2 => e1)),
        stage {
          query.flatMap(e1 => query.map(e2 => e1).map(e2 => e1))
        }.list())

      // nesting with outer macro reference
      {
        val inMemResult = for (o <- inMem; i <- inMem) yield o.name
        List(
          stage {
            query.flatMap(o => query.map(i => o.name))
            }.list(),
          // Queryable(for (o <- query; i <- query) yield o.name),
          stage {
            (for (o <- query; i <- query) yield o.name)
            }.list()).foreach {
            query_ => assertMatch(inMemResult, query_)
          }
      }

      // nesting with chaining / comprehension with cartesian product and if
      {
        val inMemResult = for (o <- inMem; i <- inMem; if i.sales == o.sales) yield i.name
        List(
          stage {
            query.flatMap(o => query.filter(i => i.sales == o.sales).map(i => i.name))
            }.list(),
          // Queryable(for (o <- query; i <- query; if i.sales == o.sales) yield i.name),
          stage {
            (for (o <- query; i <- query; if i.sales == o.sales) yield i.name)
            }.list()).foreach {
            query_ => assertMatch(inMemResult, query_)
          }
      }

      // tuples
      assertMatch(
        inMem.map(c => (c.name, c.sales)),
        stage {
          query.map(c => (c.name, c.sales))
        }.list())

      // nested structures (here tuples and case classes)
      //      assertMatch(
      //        inMem.map(c => (c.name, c.sales, c)),
      //        stageDebug {
      //          Queryable[Coffee].map(c => (c.name, c.sales, Coffee(c.name, c.sales, c.flavor))).toSeq
      //        })
      // length
      assertEquals(inMem.length, stage {
        //        Query(query.length).first
        Query(query.length)
      }.first)

      assertMatchCaseClass(
        inMem.map(c => c),
        stage {
          query.map(c => c)
        }.list())

      assertMatch(for (v1 <- inMem; v2 <- inMem; if !(v1.name == v2.name)) yield (v1.name, v2.name),
        stage {
          (for (v1 <- query; v2 <- query; if !(v1.name == v2.name)) yield (v1.name, v2.name))
        }.list())

      assertMatchCaseClass(inMem.take(2),
        stage { query.take(2)}.list())

      assertMatchCaseClass(inMem.drop(2),
        stage { query.drop(2)}.list())

      assertMatchOrdered(inMem.sortBy(_.name),
        stage { query.sortBy(_.name)}.list())

      assertMatchOrdered(inMem.sortBy(c => (c.name, c.sales)),
        stage { query.sortBy(c => (c.name, c.sales))}.list())

      assertMatchOrdered(inMem.sortBy(c => c.name)(Ordering[String].reverse),
        stage { query.sortBy(c => c.name)(Ordering[String].reverse)}.list())

      assertMatchOrdered(inMem.sortBy(c => (c.name, c.sales))(Ordering.Tuple2(Ordering[String], Ordering[Int].reverse)),
        stage { query.sortBy(c => (c.name, c.sales))(Ordering.Tuple2(Ordering[String], Ordering[Int].reverse))}.list())

      def nullOrdering(x: Int, y: Int) = new scala.math.Ordering[Option[String]] {
        def compare(a: Option[String], b: Option[String]) = {
          if (a == None && b == None) 0
          else if (a == None) x * -1
          else if (b == None) x * 1
          else y * (a.get compare b.get)
        }
      }

      stringOptionOrdering = nullOrdering(-1, 1)
      assertMatchOrdered(inMem.sortBy(c => (c.flavor, c.name)),
        stage {
          query.sortBy(c => (c.flavor, c.name))(Ordering.Tuple2(nonesLast[String], Ordering.String))
        }.list())

      stringOptionOrdering = nullOrdering(1, 1)
      assertMatchOrdered(inMem.sortBy(c => (c.flavor, c.name)),
        stage {
          query.sortBy(c => (c.flavor, c.name))(Ordering.Tuple2(nonesFirst[String], Ordering.String))
        }.list())

      stringOptionOrdering = nullOrdering(-1, -1)
      assertMatchOrdered(inMem.sortBy(c => (c.flavor, c.name)),
        stage {
          query.sortBy(c => (c.flavor, c.name))(Ordering.Tuple2(nonesLast.reverse, Ordering.String))
        }.list())

      stringOptionOrdering = nullOrdering(1, -1)
      assertMatchOrdered(inMem.sortBy(c => (c.flavor, c.name)),
        stage {
          query.sortBy(c => (c.flavor, c.name))(Ordering.Tuple2(nonesFirst.reverse, Ordering.String))
        }.list())

      import scala.slick.direct.order.reversed

      stringOptionOrdering = initialStringOptionOrdering
      assertMatchOrdered(inMem.sortBy(c => (
        c.name, reversed(c.sales), reversed(c.flavor))),
        stage {
          query.sortBy(c => (
            c.name, c.sales, c.flavor))(Ordering.Tuple3(Ordering[String], Ordering[Int].reverse, nonesFirst.reverse))
        }.list())
    }
  }

}
