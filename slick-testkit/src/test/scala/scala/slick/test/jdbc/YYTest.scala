package scala.slick.test.jdbc

import scala.language.implicitConversions
import org.junit.Test
import org.junit.Assert._
import scala.slick.yy._
import scala.slick.driver.H2Driver
import scala.slick.jdbc.JdbcBackend

//@Entity("COFFEE") case class CoffeeNotNested(@Entity("ID") idNumber: Int, @Entity("NAME") coffeeName: String)
//object NestingObject {
//  @Entity("COFFEE") case class CoffeeNested1(@Entity("ID") idNumber: Int, @Entity("NAME") coffeeName: String)
//  object Level2 {
//    @Entity("COFFEE") case class CoffeeNested2(@Entity("ID") idNumber: Int, @Entity("NAME") coffeeName: String)
//  }
//}

class YYTest {
  import scala.slick.yy.test.YYDefinitions._

  @Test def simpleTest() {
    import Shallow._
    import Shallow.TestH2._
    val y = 5.3
    val r1 = shallow {
      val q = Query(y)
      q.toSeq
    }
    assertEquals("Query of int", y, r1.head, 0.1)
    val r2 = shallow {
      Query(y).map(x => x).toSeq
    }
    assertEquals("Query identity map", y, r2.head, 0.1)
    val r3 = shallow {
      Query(y).map(x => false).toSeq
    }
    assertEquals("Query dummy map", false, r3.head)
    val r4 = shallow {
      Query(y).filter(x => x < 1).toSeq
    }
    assertEquals("Query filter", 0, r4.length)

    val z = 3
    val r5 = shallow {
      Query(z).filter(x => x > 2.5).toSeq
    }
    assertEquals("Query filter + captured var", z, r5.head)
    val a = 1
    val r6 = shallow {
      val b = 1
      Query(a).filter(x => x == b).toSeq
    }
    assertEquals("Query filter + Column ==", a, r6.head)
    val r7 = shallow {
      val b = 1
      Query(2 > b).filter(x => x == true).toSeq
    }
    assertEquals("Query filter + Column == (2)", true, r7.head)
  }
  @Test
  def tuple2Test() {
    import Shallow._
    import Shallow.TestH2._
    val r1 = shallow {
      val x = (1, 2)
      val q = Query(x)
      q.first
    }
    assertEquals("Query of tuple2", (1, 2), r1)
    val r2 = shallow {
      val x = (1, 2.5)
      Query(x).map(x => x._2).first
    }
    assertEquals("Query map _2 of tuple", 2.5, r2, 0.1)
    val r3 = shallow {
      val x = (1, 2)
      val q = Query(x).map(x => x._2)
      val q2 = q.filter(x => x == 1)
      q2.toSeq
    }
    assertEquals("Query filter of tuple2 + Column ==", 0, r3.length)
    val r4 = shallow {
      val x = (1, 2)
      Query(x).map(x => x._2).filter(x => x == 2).toSeq
    }
    assertEquals("Query filter of tuple2 + Column == (2)", 1, r4.length)
    val r5 = shallow {
      val x = (1, 2)
      Query(x).map(x => x._2).filter(x => x > 1).toSeq
    }
    assertEquals("Query filter of tuple2 + Column >", 1, r5.length)
    val r6 = shallow {
      Query((1, 2)).map(x => (x._2, if (x._2 == 2) false else true)).toSeq
    }
    assertEquals("Query map of tuple 2 + Column > + if true", (2, false), r6.head)
    val r7 = shallow {
      Query((1, 2)).map(x => (x._2, if (x._2 == 1) false else true)).toSeq
    }
    assertEquals("Query map of tuple 2 + Column > + if false", (2, true), r7.head)
  }

  @Test
  def tuple22Test() {
    import Shallow._
    import Shallow.TestH2._
    val r1 = shallow {
      val x = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
      val q = Query(x)
      q.first
    }
    assertEquals("Query of tuple22", (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22), r1)
  }

  @Test
  def virtualizationProTest {
    initCoffeeTable()
    import Shallow._
    import Shallow.TestH2._

    val r1 = shallow {
      val tbl = Table.getTable[Coffee1]
      val q = Query.ofTable(tbl)
      val q1 = q map (x => x.id)
      q1.toSeq
    }
    assertEquals("Query map _1 of Virtualized++ Table", 4, r1.length)
    val r2 = shallow {
      val q1 = Queryable[Coffee1] map (x => (x.id, x.name))
      q1.toSeq
    }
    assertEquals("Query map (_1, _2) of Virtualized++ Table", List((1, "one"), (2, "two"), (3, "three"), (10, "ten")), r2.toList)
    val r3 = shallow {
      val q1 = Queryable[Coffee1] map (x => (x.id, if (x.id < 3) "Low" else x.name))
      q1.toSeq
    }
    assertEquals("Query map (_1, _2) of Virtualized++ Table + if", List((1, "Low"), (2, "Low"), (3, "three"), (10, "ten")), r3.toList);
    val r4 = shallow {
      val q1 = Queryable[Coff] map (x => (x.idNumber, x.name))
      q1.toSeq
    }
    assertEquals("Query map (_1, _2) of Virtualized++ Table + Annotation", List((1, "one"), (2, "two"), (3, "three"), (10, "ten")), r4.toList)
    val r5 = shallow {
      val q1 = Queryable[Coff] map (x => x.idNumber) filter (x => x < 3)
      q1.toSeq
    }
    assertEquals("Query map _1 filter of Virtualized++ Table + Annotation", List(1, 2), r5.toList)
    val r6 = shallow {
      val q1 = Queryable[Coff] map (x => (x.idNumber, x.name)) filter (x => x._1 < 3)
      q1.toSeq
    }
    assertEquals("Query map (_1, _2) filter of Virtualized++ Table + Annotation", List((1, "one"), (2, "two")), r6.toList)

    val r7 = shallow {
      val q1 = Queryable[Coffn] filter (x => x.idNumber == 3) map (x => (x.idNumber, x._2))
      q1.toSeq
    }
    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", List((3, "three")), r7.toList)
    DatabaseHandler.closeSession
  }

  //  @Test
  //  def virtualizationOutsideTest {
  //    initCoffeeTable()
  //    import Shallow._
  //    import Shallow.TestH2._
  //    val r1 = shallow {
  //      val q1 = Queryable[CoffeeNotNested] filter (x => x.idNumber == 3)
  //      q1.first
  //    }
  //    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", CoffeeNotNested(3, "three"), r1)
  //    val r2 = shallow {
  //      val q1 = Queryable[CoffeeNotNested] filter (x => x.idNumber == 3)
  //      q1.toSeq
  //    }
  //    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", CoffeeNotNested(3, "three"), r2.head)
  //
  //    import NestingObject.CoffeeNested1
  //
  //    val r3 = shallow {
  //      val q1 = Queryable[CoffeeNested1] filter (x => x.idNumber == 3)
  //      q1.first
  //    }
  //    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", CoffeeNested1(3, "three"), r3)
  //    val r4 = shallow {
  //      val q1 = Queryable[CoffeeNested1] filter (x => x.idNumber == 3)
  //      q1.toSeq
  //    }
  //    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", CoffeeNested1(3, "three"), r4.head)
  //
  //    import NestingObject.Level2.CoffeeNested2
  //
  //    val r5 = shallow {
  //      val q1 = Queryable[CoffeeNested2] filter (x => x.idNumber == 3)
  //      q1.first
  //    }
  //    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", CoffeeNested2(3, "three"), r5)
  //    val r6 = shallow {
  //      val q1 = Queryable[CoffeeNested2] filter (x => x.idNumber == 3)
  //      q1.toSeq
  //    }
  //    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", CoffeeNested2(3, "three"), r6.head)
  //
  //    //    @Entity("COFFEE") case class CoffeeNested3(@Entity("ID") idNumber: Int, @Entity("NAME") coffeeName: String)
  //    //    val r7 = shallow {
  //    //      val q1 = Queryable[CoffeeNested3] filter (x => x.idNumber == 3)
  //    //      q1.first
  //    //    }
  //    //    val r8 = shallow {
  //    //      val q1 = Queryable[CoffeeNested3] filter (x => x.idNumber == 3)
  //    //      q1.toSeq
  //    //    }
  //    //    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", CoffeeNested3(3, "three"), r8.head)
  //
  //    DatabaseHandler.closeSession
  //  }

  @Test
  def sortTest {
    initSortTable()
    import Shallow._
    import Shallow.TestH2._
    val r1 = shallow {
      val q1 = Queryable[Coffee1] map (x => (x.id, x.name)) sortBy (x => x._2)
      q1.toSeq
    }
    assertEquals("Query sort by name of Table", List((2, "one"), (1, "one"), (10, "ten"), (3, "three")), r1.toList)
    val r2 = shallow {
      val q1 = Queryable[Coffee1] map (x => (x.id, x.name)) sortBy (x => x._1)
      q1.toSeq
    }
    assertEquals("Query sort by id of Table", List((1, "one"), (2, "one"), (3, "three"), (10, "ten")), r2.toList)
    val r3 = shallow {
      val q1 = Queryable[Coffee1] map (x => (x.id, x.name)) sortBy (x => (x._2, x._1))
      q1.toSeq
    }
    assertEquals("Query sort by (name, id) of Table", List((1, "one"), (2, "one"), (10, "ten"), (3, "three")), r3.toList)
    val r4 = shallow {
      val q1 = Queryable[Coffee1] map (x => (x.id, x.name)) sortBy (x => (x._2, x._1)) take 2
      q1.toSeq
    }
    assertEquals("Query sort by (name, id) + take of Table", List((1, "one"), (2, "one")), r4.toList)
    val r5 = shallow {
      val q1 = Queryable[Coffee1] map (x => (x.id, x.name)) sortBy (x => (x._1, x._2)) drop 1
      q1.toSeq
    }
    assertEquals("Query sort by (id, name) + drop of Table", List((2, "one"), (3, "three"), (10, "ten")), r5.toList)

    val r6 = shallow {
      val q1 = Queryable[Coffee1].map(x => (x.id, x.name)).sortBy(x => x._1)(Ordering[Int])
      q1.toSeq
    }
    assertEquals("Query sort by id of Table + Ordering", List((1, "one"), (2, "one"), (3, "three"), (10, "ten")), r6.toList)
    val r7 = shallow {
      val q1 = Queryable[Coffee1].map(x => (x.id, x.name)).sortBy(x => x._1)(Ordering[Int].reverse)
      q1.toSeq
    }
    assertEquals("Query sort by reverse of id of Table + Ordering", List((10, "ten"), (3, "three"), (2, "one"), (1, "one")), r7.toList)
    val r8 = shallow {
      val q1 = Queryable[Coffee1].map(x => (x.id, x.name)).sortBy(x => x._2)(Ordering[String].reverse)
      q1.toSeq
    }
    assertEquals("Query sort by reverse of name of Table + Ordering", List((3, "three"), (10, "ten"), (2, "one"), (1, "one")), r8.toList)
    val r9 = shallow {
      val q1 = Queryable[Coffee1].map(x => (x.id, x.name)).sortBy(x => (x._2, x._1))(Ordering[(String, Int)])
      q1.toSeq
    }
    assertEquals("Query sort by (name, id) of Table + Ordering", List((1, "one"), (2, "one"), (10, "ten"), (3, "three")), r9.toList)
    val r10 = shallow {
      val q1 = Queryable[Coffee1].map(x => (x.id, x.name)).sortBy(x => (x._2, x._1))(Ordering[(String, Int)].reverse)
      q1.toSeq
    }
    assertEquals("Query sort by reverse of (name, id) of Table + Ordering", List((3, "three"), (10, "ten"), (2, "one"), (1, "one")), r10.toList)
    val r11 = shallow {
      val q1 = Queryable[Coffee1].map(x => (x.id, x.name)).sortBy(x => (x._2, x._1))(Ordering.by[(String, Int), String](_._1).reverse)
      q1.toSeq
    }
    assertEquals("Query sort by reverse of name of Table + Ordering", List((3, "three"), (10, "ten"), (2, "one"), (1, "one")), r11.toList)
    val r12 = shallow {
      val q1 = Queryable[Coffee1].map(x => (x.id, x.name)).sorted(Ordering.by[(Int, String), String](_._2).reverse)
      q1.toSeq
    }
    assertEquals("Query sorted reverse of name of Table + Ordering", List((3, "three"), (10, "ten"), (2, "one"), (1, "one")), r12.toList)
    val r13 = shallow {
      val q1 = Queryable[Coffee1].map(x => (x.id, x.name)).sorted(Ordering.by[(Int, String), (String, Int)](x => (x._2, x._1)).reverse)
      q1.toSeq
    }
    assertEquals("Query sorted by reverse of (name, id) of Table + Ordering", List((3, "three"), (10, "ten"), (2, "one"), (1, "one")), r13.toList)

    DatabaseHandler.closeSession
  }

  @Test
  def forComprehensionTest() {
    initCoffeeTable()
    import Shallow._
    import Shallow.TestH2._
    val r1 = shallow {
      val tbl = Table.getTable[Coffee1]
      val q = Query.ofTable(tbl)
      val q1 = for (x <- q) yield x.id
      q1.toSeq
    }
    assertEquals("Query forComprehension map _1 of Virtualized Table", 4, r1.length)
    val r2 = shallow {
      val q1 = for (x <- Queryable[Coffee1]) yield (x.id, x.name)
      q1.toSeq
    }
    assertEquals("Query forComprehension map (_1, _2) of Table", List((1, "one"), (2, "two"), (3, "three"), (10, "ten")), r2.toList)
    val r3 = shallow {
      val q1 = for (x <- Queryable[Coffee1]) yield (x.id, if (x.id < 3) "Low" else x.name)
      q1.toSeq
    }
    assertEquals("Query forComprehension map (_1, _2) of Table + if", List((1, "Low"), (2, "Low"), (3, "three"), (10, "ten")), r3.toList)
    val r4 = shallow {
      val q1 = for (x <- Queryable[Coff]) yield (x.idNumber, x.name)
      q1.toSeq
    }
    assertEquals("Query forComprehension map (_1, _2) of Table + Annotation", List((1, "one"), (2, "two"), (3, "three"), (10, "ten")), r4.toList)
    val r5 = shallow {
      val q1 = for (x <- Queryable[Coff] if x.idNumber < 3) yield x.idNumber
      q1.toSeq
    }
    assertEquals("Query forComprehension map _1 filter of Table + Annotation", List(1, 2), r5.toList)
    val r6 = shallow {
      val q1 = for (x <- Queryable[Coff] if x.idNumber < 3) yield (x.idNumber, x.name)
      q1.toSeq
    }
    assertEquals("Query forComprehension map (_1, _2) filter of Table + Annotation", List((1, "one"), (2, "two")), r6.toList)
    val r7 = shallow {
      val q1 = for (x <- Queryable[Coffn] if x.idNumber == 3) yield (x.idNumber, x._2)
      q1.toSeq
    }
    assertEquals("Query forComprehension filter == map (_1, _2) of Table + Annotation", List((3, "three")), r7.toList)
    val r8 = shallow {
      val q1 = for ((x, y) <- Queryable[Coffn].map(x => (x.idNumber, x._2)) if x == 3) yield (x, y)
      q1.toSeq
    }
    assertEquals("Query forComprehension filter == map (_1, _2) of Table + Annotation + (_1, _2) <-", List((3, "three")), r8.toList)
    val r9 = shallow {
      val q1 = for (x <- Queryable[Coff]) yield (x.idNumber, (x.idNumber, x.name))
      q1.toSeq
    }
    assertEquals("Query forComprehension map NestedTuples (_1, (_1, _2)) filter of Table + Annotation", List((1, (1, "one")), (2, (2, "two")), (3, (3, "three")), (10, (10, "ten"))), r9.toList)
    DatabaseHandler.closeSession
  }
  @Test
  def virtualizationProInvokerTest {
    initCoffeeTable()
    import Shallow._
    def driver = H2Driver
    implicit val session = DatabaseHandler.provideSession
    val r1 = shallow {
      val q1 = Queryable[Coffee1] map (x => x.id)
      q1.getInvoker
    }(driver)
    assertEquals("Query map _1 of Virtualized++ Table invoker", 4, r1.list.length)
    val r2 = shallow {
      val q1 = Queryable[Coffee1] map (x => x.id)
      q1.toSeqImplicit
    }(driver)(session)
    assertEquals("Query map _1 of Virtualized++ Table toSeqImplicit", 4, r2.length)
    DatabaseHandler.closeSession
  }

  @Test
  def columnOpsTest {
    initCoffeeTable()
    import Shallow._
    import Shallow.TestH2._
    val r1 = shallow {
      val q1 = Queryable[Coffee1] map (x => x.id + 2)
      q1.toSeq
    }
    assertEquals("numericOps +", List(3, 4, 5, 12), r1.toList)
    val r2 = shallow {
      val q1 = Queryable[Coffee1] map (x => x.id * 2 % 3)
      q1.toSeq
    }
    assertEquals("numericOps * %", List(2, 1, 0, 2), r2.toList)
    val r3 = shallow {
      val q1 = Queryable[Coffee1] map (x => ((x.id - 5).abs, (x.id).toDegrees))
      q1.toSeq
    }
    assertEquals("numericOps (x - 5).abs, toDegrees", List((4, 57), (3, 115), (2, 172), (5, 573)), r3.toList)
    val r4 = shallow {
      val q1 = Queryable[Coffee1] map (x => (x.name + "!"))
      q1.toSeq
    }
    assertEquals("stringOps +", List("one!", "two!", "three!", "ten!"), r4.toList)
    val r5 = shallow {
      val q1 = Queryable[Coffee1] map (x => (x.name ++ "!").toUpperCase)
      q1.toSeq
    }
    assertEquals("stringOps ++ toUpperCase", List("ONE!", "TWO!", "THREE!", "TEN!"), r5.toList)
    val r6 = shallow {
      val q1 = Queryable[Coffee1] map (x => if (x.name like "%e") x.name.toUpperCase else ("  " + x.name + "! ").trim)
      q1.toSeq
    }
    assertEquals("stringOps if (like %%e) toUpperCase else ( + + ).trim", List("ONE", "two!", "THREE", "ten!"), r6.toList)
    val r7 = shallow {
      val q1 = Queryable[Coffee1] map (x => if (x.name like "%e") ("  " + x.name + "!  ").ltrim else ("  " + x.name + "!  ").rtrim)
      q1.toSeq
    }
    assertEquals("stringOps if (like %%e) ( + + ).ltrim else ( + + ).rtrim", List("one!  ", "  two!", "three!  ", "  ten!"), r7.toList)
    val r8 = shallow {
      val q1 = Queryable[Coffee1] map (x => if (x.name endsWith "e") x.name.toUpperCase else ("  " + x.name + "! ").trim)
      q1.toSeq
    }
    assertEquals("stringOps if (endsWith 'e') toUpperCase else ( + + ).trim", List("ONE", "two!", "THREE", "ten!"), r8.toList)
    DatabaseHandler.closeSession
  }
  @Test
  def testJoin {
    import Shallow.TestH2._
    {
      import scala.slick.driver.H2Driver.simple._
      object Categories extends Table[(Int, String)]("cat_j") {
        def id = column[Int]("id")
        def name = column[String]("name")
        def * = id ~ name
      }

      object Posts extends Table[(Int, String, Int)]("posts_j") {
        def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def title = column[String]("title")
        def category = column[Int]("category")
        def * = id ~ title ~ category
      }

      (Categories.ddl ++ Posts.ddl).create

      Categories insertAll (
        (1, "Scala"),
        (2, "ScalaQuery"),
        (3, "Windows"),
        (4, "Software"))
      Posts.title ~ Posts.category insertAll (
        ("Test Post", -1),
        ("Formal Language Processing in Scala, Part 5", 1),
        ("Efficient Parameterized Queries in ScalaQuery", 2),
        ("Removing Libraries and HomeGroup icons from the Windows 7 desktop", 3),
        ("A ScalaQuery Update", 2))
    }

    import Shallow._

    val q1 = shallow {
      (for {
        c <- Queryable[Categories]
        p <- Queryable[Posts] if c.id == p.category
      } yield (p.id, c.id, c.name, p.title)).sortBy(_._1).map(x => (x._1, x._2)).toSeq
    }
    assertEquals(List((2, 1), (3, 2), (4, 3), (5, 2)), q1.toList)

    val q2 = shallow {
      val q = Queryable[Categories] innerJoin Queryable[Posts] on (_.id == _.category)
      val q1 = q.sortBy(_._2.id)
      val q2 = q1.map(x => (x._2.id, x._1.id))
      q2.toSeq
    }
    assertEquals(List((2, 1), (3, 2), (4, 3), (5, 2)), q2.toList)

    val q3 = shallow {
      (for {
        (c, p) <- Queryable[Categories] leftJoin Queryable[Posts] on (_.id == _.category)
      } yield (p.id, (p.id.?.getOrElse(0), c.id, c.name, p.title.?.getOrElse("")))).sortBy(x => x._1)(nullsFirst).map(_._2).map(x => (x._1, x._2)).toSeq
    }
    assertEquals(List((0, 4), (2, 1), (3, 2), (4, 3), (5, 2)), q3.toList)

    assertFail {
      shallow {
        (for {
          (c, p) <- Queryable[Categories] leftJoin Queryable[Posts] on (_.id == _.category)
        } yield (p.id, c.id, c.name, p.title)).sortBy(_._1)(nullsFirst).toSeq
      }
    }

    val q3b = shallow {
      (for {
        (c, p) <- Queryable[Categories] leftJoin Queryable[Posts] on (_.id == _.category)
      } yield (p.id, (p.id.?.getOrElse(0), c.id, c.name, p.title.?.getOrElse("")))).sortBy(_._1)(nullsLast).map(_._2).map(x => (x._1, x._2)).toSeq
    }
    assertEquals(List((2, 1), (3, 2), (4, 3), (5, 2), (0, 4)), q3b.toList)

    val q4 = shallow {
      (for {
        (c, p) <- Queryable[Categories] rightJoin Queryable[Posts] on (_.id == _.category)
      } yield (p.id, c.id.?.getOrElse(0), c.name.?.getOrElse(""), p.title)).sortBy(_._1).map(x => (x._1, x._2)).toSeq
    }
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3), (5, 2)), q4.toList)
    DatabaseHandler.closeSession
  }

  @Test
  def testZip = {
    import Shallow.TestH2._
    {
      import scala.slick.driver.H2Driver.simple._
      object Categories extends Table[(Int, String)]("cat_j") {
        def id = column[Int]("id")
        def name = column[String]("name")
        def * = id ~ name
      }

      object Posts extends Table[(Int, String, Int)]("posts_j") {
        def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def title = column[String]("title")
        def category = column[Int]("category")
        def * = id ~ title ~ category
      }

      (Categories.ddl ++ Posts.ddl).create

      Categories insertAll (
        (1, "Scala"),
        (3, "Windows"),
        (2, "ScalaQuery"),
        (4, "Software"))
      Posts.title ~ Posts.category insertAll (
        ("Test Post", -1),
        ("Formal Language Processing in Scala, Part 5", 1),
        ("Efficient Parameterized Queries in ScalaQuery", 2),
        ("Removing Libraries and HomeGroup icons from the Windows 7 desktop", 3),
        ("A ScalaQuery Update", 2))
    }

    import Shallow._
    val q1 = shallow {
      val q = Queryable[Categories].sortBy(_.id).zipWithIndex
      val q1 = q.map(x => (x._1.id, x._2))
      q1.toSeq
    }
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3)), q1.toList)
    val q2 = shallow {
      val q =
        Queryable[Categories].sortBy(_.id) zip Queryable[Posts].sortBy(_.category)
      val q1 = q.map(x => (x._1.id, x._2.category))
      q1.toSeq
    }
    assertEquals(List((1, -1), (2, 1), (3, 2), (4, 2)), q2.toList)
    val q3 = shallow {
      val q = for {
        (c, p) <- Queryable[Categories].sortBy(_.id) zip Queryable[Posts].sortBy(_.category)
      } yield (c.id, p.category)
      q.toSeq
    }
    assertEquals(List((1, -1), (2, 1), (3, 2), (4, 2)), q3.toList)
    DatabaseHandler.closeSession
  }

  @Test
  def testGroupBy = {
    import Shallow.TestH2._
    {
      import scala.slick.driver.H2Driver.simple._
      object T extends Table[(Int, Int)]("t3") {
        def a = column[Int]("a")
        def b = column[Int]("b")
        def * = a ~ b
      }
      T.ddl.create
      T.insertAll((1, 1), (1, 2), (1, 3))
      T.insertAll((2, 1), (2, 2), (2, 5))
      T.insertAll((3, 1), (3, 9))
    }
    import Shallow._
    val r0 = shallow {
      val q0 = Queryable[T3].groupBy(_.a)
      val q1 = q0.map(_._2.length).sorted
      q1.toSeq
    }
    val r0t: List[Int] = r0.toList
    assertEquals(List(2, 3, 3), r0t)
    val r = shallow {
      (for {
        (k, v) <- Queryable[T3].groupBy(t => t.a)
      } yield (k, v.length)).sortBy(_._1).toSeq
    }
    val rt: List[(Int, Int)] = r.toList
    assertEquals(List((1, 3), (2, 3), (3, 2)), rt)

    val r2 = shallow {
      (
        (for {
          (k, v) <- Queryable[T3].groupBy(t => t.a)
        } yield (k, v.length, v.map(_.a).sum, v.map(_.b).sum)).sortBy(_._1)
      ).toSeq
    }
    val r2t: List[(Int, Int, Option[Int], Option[Int])] = r2.toList
    assertEquals(List((1, 3, Some(3), Some(6)), (2, 3, Some(6), Some(8)), (3, 2, Some(6), Some(10))), r2t)
    DatabaseHandler.closeSession
  }

  @Test
  def testOption = {
    import Shallow.TestH2._
    val inMemT3 = List((1, Some(1)), (-1, Some(2)), (1, Some(3)),
      (2, Some(1)), (2, Some(2)), (-2, Some(5)),
      (-3, Some(1)), (3, None)
    )

    {
      import scala.slick.driver.H2Driver.simple._
      object T extends Table[(Int, Option[Int])]("t3o") {
        def a = column[Int]("a")
        def b = column[Option[Int]]("b")
        def * = a ~ b
      }
      T.ddl.create
      T.insertAll(inMemT3: _*)
    }
    import Shallow._
    val r0 = shallow {
      Queryable[T3O].map(x => (x.a, x.b)).toSeq
    }
    assertEquals(inMemT3, r0.toList)
    val r1 = shallow {
      Queryable[T3O].map(x => (x.a, x.b.getOrElse(0))).toSeq
    }
    assertEquals(inMemT3 map {
      case (_1, _2) => (_1, _2.getOrElse(0))
    }, r1.toList)
    assertEquals(inMemT3, r0.toList)
    val r2 = shallow {
      Queryable[T3O].map(x => (x.a.?, x.b.getOrElse(0))).toSeq
    }
    assertEquals(inMemT3 map {
      case (_1, _2) => (Some(_1), _2.getOrElse(0))
    }, r2.toList)
    val r3 = shallow {
      Queryable[T3O].map(x => (x.a.?.getOrElse(3), x.b.getOrElse(0))).toSeq
    }
    assertEquals(inMemT3 map {
      case (_1, _2) => (_1, _2.getOrElse(0))
    }, r3.toList)
    val r4 = shallow {
      Queryable[T3O].map(x => (x.a.?.toRadians, x.b.getOrElse(0))).toSeq
    }
    assertEquals(inMemT3 map {
      case (_1, _2) => (Some(0), _2.getOrElse(0))
    }, r4.toList)
    DatabaseHandler.closeSession
  }

  @Test
  def composabilityTest {
    initCoffeeTable()
    import Shallow._
    import Shallow.TestH2._

    {
      import scala.slick.driver.H2Driver.simple._
      object Categories extends Table[(Int, String)]("cat_j") {
        def id = column[Int]("id")
        def name = column[String]("name")
        def * = id ~ name
      }

      object Posts extends Table[(Int, String, Int)]("posts_j") {
        def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def title = column[String]("title")
        def category = column[Int]("category")
        def * = id ~ title ~ category
      }

      (Categories.ddl ++ Posts.ddl).create

      Categories insertAll (
        (1, "Scala"),
        (2, "ScalaQuery"),
        (3, "Windows"),
        (4, "Software"))
      Posts.title ~ Posts.category insertAll (
        ("Test Post", -1),
        ("Formal Language Processing in Scala, Part 5", 1),
        ("Efficient Parameterized Queries in ScalaQuery", 2),
        ("Removing Libraries and HomeGroup icons from the Windows 7 desktop", 3),
        ("A ScalaQuery Update", 2))

      object T extends Table[(Int, Int)]("t3") {
        def a = column[Int]("a")
        def b = column[Int]("b")
        def * = a ~ b
      }
      T.ddl.create
      T.insertAll((1, 1), (1, 2), (1, 3))
      T.insertAll((2, 1), (2, 2), (2, 5))
      T.insertAll((3, 1), (3, 9))
    }

    val q = shallow {
      Queryable[Coffee1] filter (x => x.id == 3)
    }
    val r7 = shallow {
      q.toSeq
    }
    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", Coffee1(3, "three"), r7.head)

    val qq = shallow {
      Queryable[Coffee1] map (x => x.id)
    }
    val r8 = shallow {
      (qq filter (x => x < 3)).toSeq
    }
    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", List(1, 2), r8.toList)

    val q0 = shallow {
      Queryable[Coffee1]
    }
    val q1 = shallow {
      q0 filter (x => x.id == 3)
    }
    val r9 = shallow {
      q1.toSeq
    }
    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", Coffee1(3, "three"), r9.head)

    val categories = shallow {
      Queryable[Categories]
    }
    val posts = shallow {
      Queryable[Posts]
    }

    val cpImplicitJoin = shallow {
      for {
        c <- Queryable[Categories]
        p <- Queryable[Posts] if c.id == p.category
      } yield (p.id, c.id, c.name, p.title)
    }

    val cp1 = shallow {
      (cpImplicitJoin).sortBy(_._1).map(x => (x._1, x._2)).toSeq
    }
    assertEquals(List((2, 1), (3, 2), (4, 3), (5, 2)), cp1.toList)

    val cpInnerJoin = shallow {
      categories innerJoin posts on (_.id == _.category)
    }
    val cpInnerJoinSorted = shallow {
      cpInnerJoin.sortBy(_._2.id)
    }
    val cpInnerJoinMapped = shallow {
      cpInnerJoinSorted.map(x => (x._2.id, x._1.id))
    }
    val cp2 = shallow {
      cpInnerJoinMapped.toSeq
    }
    assertEquals(List((2, 1), (3, 2), (4, 3), (5, 2)), cp2.toList)

    val cpRightJoin = shallow {
      categories rightJoin posts on (_.id == _.category)
    }
    val cpRightJoinMapped = shallow {
      for {
        (c, p) <- cpRightJoin
      } yield (p.id, c.id.?.getOrElse(0), c.name.?.getOrElse(""), p.title)
    }
    val cpRightJoinSorted = shallow {
      cpRightJoinMapped.sortBy(_._1)
    }
    val cpRightJoinSortedMapped = shallow {
      cpRightJoinSorted.map(x => (x._1, x._2))
    }
    val cp3 = shallow {
      cpRightJoinSortedMapped.toSeq
    }
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3), (5, 2)), cp3.toList)

    val t3Query = shallow {
      Queryable[T3]
    }
    val t3GroupBy = shallow {
      t3Query.groupBy(t => t.a)
    }
    val t3GroupByMapped = shallow {
      for {
        (k, v) <- t3GroupBy
      } yield (k, v.length, v.map(_.a).sum, v.map(_.b).sum)
    }
    val t3GroupBySorted = shallow {
      t3GroupByMapped.sortBy(_._1)
    }
    val t3r = shallow {
      t3GroupBySorted.toSeq
    }
    val t3result: List[(Int, Int, Option[Int], Option[Int])] = t3r.toList
    assertEquals(List((1, 3, Some(3), Some(6)), (2, 3, Some(6), Some(8)), (3, 2, Some(6), Some(10))), t3result)
    DatabaseHandler.closeSession
  }

  def initCoffeeTable() {
    import scala.slick.driver.H2Driver.simple._

    object Coffee extends Table[(Int, String)]("COFFEE") {
      def id = column[Int]("ID")
      def name = column[String]("NAME")
      def * = id ~ name
    }

    object Test extends YYSlickCake {
      implicit val session = DatabaseHandler.provideSession

      (Coffee.ddl).create

      Coffee.insert((1, "one"))
      Coffee.insert((2, "two"))
      Coffee.insert((3, "three"))
      Coffee.insert((10, "ten"))
    }
    Test
  }

  def initSortTable() {
    import scala.slick.driver.H2Driver.simple._

    object Coffee extends Table[(Int, String)]("COFFEE") {
      def id = column[Int]("ID")
      def name = column[String]("NAME")
      def * = id ~ name
    }

    object Test extends YYSlickCake {
      implicit val session = DatabaseHandler.provideSession

      (Coffee.ddl).create

      Coffee.insert((2, "one"))
      Coffee.insert((1, "one"))
      Coffee.insert((3, "three"))
      Coffee.insert((10, "ten"))
    }
    Test
  }

  def assertFail(f: => Unit) = {
    var succeeded = false
    try {
      f
      succeeded = true
    } catch {
      case e: Exception if !scala.util.control.Exception.shouldRethrow(e) =>
    }
    if (succeeded) fail("Exception expected")
  }

  val DatabaseHandler = Shallow.TestH2
}
