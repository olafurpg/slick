package scala.slick.test.jdbc

import scala.language.{ reflectiveCalls, implicitConversions }
import org.junit.Test
import org.junit.Assert._
import scala.slick.testutil._
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag
import com.typesafe.slick.testkit.util.TestDB
import slick.jdbc.StaticQuery.interpolation
import scala.slick.SlickException
import scala.slick.yy._
import ch.qos.logback.core.pattern.util.AsIsEscapeUtil
import scala.slick.yy.test.YYDefinitions._

object ShallowTest extends DBTestObject(TestDBs.H2Mem, TestDBs.H2Disk, TestDBs.HsqldbMem, TestDBs.HsqldbDisk, TestDBs.SQLiteMem, TestDBs.SQLiteDisk /*, TestDBs.DerbyMem, TestDBs.DerbyDisk*/ )

class ShallowTest(val tdb: TestDB) extends DBTest {
  implicit val testDriver = tdb.driver
  implicit val testSession = tdb.createDB().createSession

  object Singleton {
    import Shallow._
    // TODO needs some fixes in YY for ScopeInjection to be able to use composibility
    val q = shallow {
      Queryable[Coffee].toSeq
    }
    val q1 = shallow {
      Queryable[Coffee].map(_.sales + 5).toSeq
    }
    object Singleton {
      val q = shallow {
        Queryable[Coffee].toSeq
      }
      val q1 = shallow {
        Queryable[Coffee].map(_.sales + 5).toSeq
      }
      object Singleton {
        val q = shallow {
          Queryable[Coffee].toSeq
        }
        val q1 = shallow {
          Queryable[Coffee].map(_.sales + 5).toSeq
        }
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
    val q1 = shallow {
      Queryable[Coffee].map(_.sales + 5).toSeq
    }
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
      val query = shallow {
        Queryable[Coffee]
      }

      // test framework sanity checks
      assertNoMatch(inMem ++ inMem, shallow { query.toSeq })
      assertNoMatch(Vector(), shallow { query.toSeq })

      // fetch whole table
      assertMatchCaseClass(inMem, shallow { query.toSeq })
      // FIXME: make this test less artificial
      class MyQuerycollection {
        def findUserByName(name: String) = shallow {
          query.filter(_.name == name).toSeq
        }
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
        shallow {
          query.map(_.sales + 5).toSeq
        })

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
        shallow {
          query.map(_.name + ".").toSeq
        })

      // filter with more complex condition
      assertMatchCaseClass(
        inMem.filter(c => c.sales > 5 || "Chris" == c.name),
        shallow {
          query.filter(c => c.sales > 5 || "Chris" == c.name).toSeq
        })

      // type annotations FIXME canBuildFrom
      assertMatch(
        inMem.map((_: Coffee).name: String),
        shallow {
          query.map[String](_.name: String).toSeq
        })

      // chaining
      assertMatch(
        inMem.map(_.name).filter(_ == ""),
        shallow {
          query.map(_.name).filter(_ == "").toSeq
        })

      // referenced values are inlined as constants using reflection
      val o = 2 + 3
      assertMatchCaseClass(
        inMem.filter(_.sales > o),
        shallow {
          query.filter(_.sales > o).toSeq
        })

      // nesting (not supported yet: query.map(e1 => query.map(e2=>e1))) 
      assertMatchCaseClass(
        inMem.flatMap(e1 => inMem.map(e2 => e1)),
        shallow {
          query.flatMap(e1 => query.map(e2 => e1)).toSeq
        })

      // query scope
      {
        val inMemResult = inMem.filter(_.sales > 5)
        List(
          shallow {
            query.filter(_.sales > 5).toSeq
          },
          // Queryable( query.filter( _.sales > 5 ) ),
          shallow {
            val foo = query
            val bar = foo.filter(_.sales > 5)
            bar.toSeq
          }).foreach {
            query_ => assertMatchCaseClass(inMemResult, query_)
          }
      }

      // comprehension with map
      assertMatch(
        for (c <- inMem) yield c.name,
        shallow {
          (for (c <- query) yield c.name).toSeq
        })

      // nesting with flatMap
      {
        val inMemResult = for (o <- inMem; i <- inMem) yield i.name
        List(
          shallow {
            query.flatMap(o => query.map(i => i.name)).toSeq
          },
          // Queryable(for (o <- query; i <- query) yield i.name), 
          shallow {
            (for (o <- query; i <- query) yield i.name).toSeq
          }).foreach {
            query_ => assertMatch(inMemResult, query_)
          }
      }

      assertMatchCaseClass(
        inMem.flatMap(e1 => inMem.map(e2 => e1).map(e2 => e1)),
        shallow {
          query.flatMap(e1 => query.map(e2 => e1).map(e2 => e1)).toSeq
        })

      // nesting with outer macro reference
      {
        val inMemResult = for (o <- inMem; i <- inMem) yield o.name
        List(
          shallow {
            query.flatMap(o => query.map(i => o.name)).toSeq
          },
          // Queryable(for (o <- query; i <- query) yield o.name),
          shallow {
            (for (o <- query; i <- query) yield o.name).toSeq
          }).foreach {
            query_ => assertMatch(inMemResult, query_)
          }
      }

      // nesting with chaining / comprehension with cartesian product and if
      {
        val inMemResult = for (o <- inMem; i <- inMem; if i.sales == o.sales) yield i.name
        List(
          shallow {
            query.flatMap(o => query.filter(i => i.sales == o.sales).map(i => i.name)).toSeq
          },
          // Queryable(for (o <- query; i <- query; if i.sales == o.sales) yield i.name),
          shallow {
            (for (o <- query; i <- query; if i.sales == o.sales) yield i.name).toSeq
          }).foreach {
            query_ => assertMatch(inMemResult, query_)
          }
      }

      // tuples
      assertMatch(
        inMem.map(c => (c.name, c.sales)),
        shallow {
          query.map(c => (c.name, c.sales)).toSeq
        })

      // nested structures (here tuples and case classes)
      //      assertMatch(
      //        inMem.map(c => (c.name, c.sales, c)),
      //        shallowDebug {
      //          Queryable[Coffee].map(c => (c.name, c.sales, Coffee(c.name, c.sales, c.flavor))).toSeq
      //        })
      // length
      assertEquals(inMem.length, shallow {
        Query(query.length).first
      })

      assertMatchCaseClass(
        inMem.map(c => c),
        shallow {
          query.map(c => c).toSeq
        })

      assertMatch(for (v1 <- inMem; v2 <- inMem; if !(v1.name == v2.name)) yield (v1.name, v2.name),
        shallow {
          (for (v1 <- query; v2 <- query; if !(v1.name == v2.name)) yield (v1.name, v2.name)).toSeq
        })

      assertMatchCaseClass(inMem.take(2),
        shallow { query.take(2).toSeq })

      assertMatchCaseClass(inMem.drop(2),
        shallow { query.drop(2).toSeq })

      assertMatchOrdered(inMem.sortBy(_.name),
        shallow { query.sortBy(_.name).toSeq })

      assertMatchOrdered(inMem.sortBy(c => (c.name, c.sales)),
        shallow { query.sortBy(c => (c.name, c.sales)).toSeq })

      assertMatchOrdered(inMem.sortBy(c => c.name)(Ordering[String].reverse),
        shallow { query.sortBy(c => c.name)(Ordering[String].reverse).toSeq })

      assertMatchOrdered(inMem.sortBy(c => (c.name, c.sales))(Ordering.Tuple2(Ordering[String], Ordering[Int].reverse)),
        shallow { query.sortBy(c => (c.name, c.sales))(Ordering.Tuple2(Ordering[String], Ordering[Int].reverse)).toSeq })

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
        shallow {
          query.sortBy(c => (c.flavor, c.name))(Ordering.Tuple2(nonesLast[String], Ordering.String)).toSeq
        })

      stringOptionOrdering = nullOrdering(1, 1)
      assertMatchOrdered(inMem.sortBy(c => (c.flavor, c.name)),
        shallow {
          query.sortBy(c => (c.flavor, c.name))(Ordering.Tuple2(nonesFirst[String], Ordering.String)).toSeq
        })

      stringOptionOrdering = nullOrdering(-1, -1)
      assertMatchOrdered(inMem.sortBy(c => (c.flavor, c.name)),
        shallow {
          query.sortBy(c => (c.flavor, c.name))(Ordering.Tuple2(nonesLast.reverse, Ordering.String)).toSeq
        })

      stringOptionOrdering = nullOrdering(1, -1)
      assertMatchOrdered(inMem.sortBy(c => (c.flavor, c.name)),
        shallow {
          query.sortBy(c => (c.flavor, c.name))(Ordering.Tuple2(nonesFirst.reverse, Ordering.String)).toSeq
        })

      import scala.slick.direct.order.reversed

      stringOptionOrdering = initialStringOptionOrdering
      assertMatchOrdered(inMem.sortBy(c => (
        c.name, reversed(c.sales), reversed(c.flavor))),
        shallow {
          query.sortBy(c => (
            c.name, c.sales, c.flavor))(Ordering.Tuple3(Ordering[String], Ordering[Int].reverse, nonesFirst.reverse)).toSeq
        })
    }
  }
}
