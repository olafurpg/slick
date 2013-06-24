package scala.slick.benchmark

import org.scalameter.api._
import scala.slick.yy.Shallow
import scala.slick.yy.YYSlickCake
import scala.slick.yy.shallow
import scala.slick.yy.shallowTemplate
import scala.slick.yy.shallowTemplateDebug
import scala.slick.yy.templateMaker

object PerformanceBenchmark extends PerformanceTest {
  lazy val executor = LocalExecutor(
    new Executor.Warmer.Default,
    Aggregator.median,
    //    new Measurer.IgnoringGC with Measurer.PeriodicReinstantiation with Measurer.OutlierElimination with Measurer.RelativeNoise)
    new Measurer.Default)
  //  lazy val reporter = Reporter.Composite(
  //    //      LoggingReporter(),
  //    new RegressionReporter(
  //      RegressionReporter.Tester.Accepter(),
  //      RegressionReporter.Historian.Complete()),
  //    HtmlReporter(true)
  //  )
  //  lazy val reporter = HtmlReporter(true)
  lazy val reporter = Reporter.Composite(LoggingReporter(), ChartReporter(ChartReporter.ChartFactory.XYLine()))
  lazy val persistor = Persistor.None
  //  lazy val persistor = new SerializationPersistor()

  //  val sizes = Gen.range("size")(10, 1500, 50)
  val sizes = Gen.range("size")(10, 260, 50)
  //  val directSizes = Gen.range("size")(1, 10, 1)
  val directSizes = Gen.range("size")(1, 2, 1)

  val ranges = for {
    size <- sizes
    //  } yield size to size
  } yield 0 until size

  val directRanges = for {
    size <- directSizes
    //  } yield size to size
  } yield 0 until size

  val DatabaseHandler = Shallow.TestH2

  initCoffeeTable()
  /*
  performance of "Templates" in {
    import Shallow.TestH2.h2Session
    measure method "Select * where id < ?" in {
      {
        import scala.slick.driver.H2Driver.simple._
        import LiftedEmbeddingDefs._
        {
          using(ranges) curve ("lifted embedding") in { r =>
            for (i <- r) {
              (for (c <- Coffee if c.id < i.bind) yield c).list
            }
          }
        }
        {
          using(ranges) curve ("lifted embedding template") in { r =>
            for (i <- r) {
              val template = (for (i <- Parameters[Int]; c <- Coffee if c.id < i) yield c)
              template(i).list
            }
          }
        }
        {
          val template = (for (i <- Parameters[Int]; c <- Coffee if c.id < i) yield c)
          using(ranges) curve ("lifted embedding template++") in { r =>
            for (i <- r) {
              template(i).list
            }
          }
        }
      }
      {
        import Shallow.TestH2.h2Driver
        import Shallow._
        import ShallowEmbeddingDefs._
        // NOT CORRECT! but very fast ;)
        //        {
        //          using(ranges) curve ("shallow embedding") in { r =>
        //            for (i <- r) {
        //              shallow {
        //                (for (c <- Queryable[Coffee] if c.id < i) yield c).toSeq
        //              }
        //            }
        //          }
        //        }
        {
          using(ranges) curve ("shallow embedding") in { r =>
            for (i <- r) {
              shallowTemplate {
                (for (c <- Queryable[Coffee] if c.id < i) yield c).toSeq
              }
            }

          }
        }
        {
          val i = 1
          val templ = shallowTemplate {
            (for (c <- Queryable[Coffee] if c.id < i) yield c).getQueryTemplate[Int]
          }
          using(ranges) curve ("shallow embedding template") in { r =>
            for (i <- r) {
              shallowTemplate {
                templ(i): Seq[Coffee]
              }
            }

          }
        }
        {
          val templ = templateMaker { (i: Int) =>
            (for (c <- Queryable[Coffee] if c.id < i) yield c).funcTemplate
          }
          using(ranges) curve ("shallow embedding template++") in { r =>
            for (i <- r) {
              shallowTemplate {
                templ(i): Seq[Coffee]
              }
            }

          }
        }
        {
          using(ranges) curve ("shallow embedding template#") in { r =>
            def template(id: Int) = shallowTemplate {
              (for (c <- Queryable[Coffee] if c.id < id) yield c).toSeq
            }
            for (i <- r) {
              template(i)
            }

          }
        }
      }
      {
        import PlainSqlDefs._
        import scala.slick.jdbc.GetResult
        implicit val getCoffeeResult = GetResult(c => new Coffee(c.<<, c.<<))
        import Q.interpolation
        {
          def template(id: Int) = sql"select id, name from Coffee where id < $id".as[Coffee]
          using(ranges) curve ("plain sql") in { r =>
            for (i <- r) {
              template(i).list()
            }
          }
        }
      }
      {
        import scala.slick.direct._
        import DirectEmbeddingDefs._

        object backend extends SlickBackend(Shallow.TestH2.h2Driver, AnnotationMapper)

        {
          using(directRanges) curve ("direct embedding") in { r =>
            for (i <- r) {
              val q = Queryable[Coffee]
              val q1 = q.filter(_.id < i)
              backend.result(q1, Shallow.TestH2.h2Session)
            }
          }
        }
      }
    }
  }
*/
  performance of "Templates simple" in {
    import Shallow.TestH2.h2Session
    measure method "Select * where id == 1" in {
      {
        import scala.slick.driver.H2Driver.simple._
        import LiftedEmbeddingDefs._
        {
          using(ranges) curve ("lifted embedding") in { r =>
            for (i <- r) {
              (for (c <- Coffee if c.id === 1) yield c).list
            }
          }
        }
        {
          val template = (for (c <- Coffee if c.id === 1) yield c)
          using(ranges) curve ("lifted embedding value") in { r =>
            for (i <- r) {
              template.list
            }
          }
        }
        {
          def template = (for (c <- Coffee if c.id === 1) yield c).list
          using(ranges) curve ("lifted embedding function") in { r =>
            for (i <- r) {
              template
            }
          }
        }
      }
      {
        import Shallow.TestH2.h2Driver
        import Shallow._
        import ShallowEmbeddingDefs._
        {
          using(ranges) curve ("shallow embedding") in { r =>
            for (i <- r) {
              shallowTemplate {
                (for (c <- Queryable[Coffee] if c.id == 1) yield c).toSeq
              }
            }
          }
        }
        {
          val templ = shallowTemplate {
            (for (c <- Queryable[Coffee] if c.id == 1) yield c)
          }
          using(ranges) curve ("shallow embedding value") in { r =>
            for (i <- r) {
              shallowTemplate {
                templ.toSeq
              }
            }
          }
        }
        {
          using(ranges) curve ("shallow embedding function") in { r =>
            def template = shallowTemplate {
              (for (c <- Queryable[Coffee] if c.id == 1) yield c).toSeq
            }
            for (i <- r) {
              template
            }

          }
        }
      }
      {
        import PlainSqlDefs._
        import scala.slick.jdbc.GetResult
        implicit val getCoffeeResult = GetResult(c => new Coffee(c.<<, c.<<))
        import Q.interpolation
        {
          def template = sql"select id, name from Coffee where id = 1".as[Coffee]
          using(ranges) curve ("plain sql") in { r =>
            for (i <- r) {
              template.list()
            }
          }
        }
      }
      {
        import scala.slick.direct._
        import DirectEmbeddingDefs._

        object backend extends SlickBackend(Shallow.TestH2.h2Driver, AnnotationMapper)

        {
          using(directRanges) curve ("direct embedding") in { r =>
            for (i <- r) {
              val q = Queryable[Coffee]
              backend.result(q, Shallow.TestH2.h2Session)
            }
          }
        }
      }
    }
  }

  def initCoffeeTable() {
    import scala.slick.driver.H2Driver.simple._
    import LiftedEmbeddingDefs._

    implicit val session = DatabaseHandler.provideSession

    (Coffee.ddl).create

    for (i <- 1 to 3000) {
      Coffee.insert((i, s"$i"))
    }
  }

  object DirectEmbeddingDefs {
    import scala.slick.direct.AnnotationMapper._
    @table case class Coffee(@column id: Int, @column name: String)
  }
  object LiftedEmbeddingDefs {
    import scala.slick.driver.H2Driver.simple._

    object Coffee extends Table[(Int, String)]("COFFEE") {
      def id = column[Int]("ID")
      def name = column[String]("NAME")
      def * = id ~ name
    }
  }
  object ShallowEmbeddingDefs {
    type Coffee = scala.slick.yy.test.YYDefinitions.Coffee1
    val Coffee = scala.slick.yy.test.YYDefinitions.Coffee1
  }
  object PlainSqlDefs {
    val Q = scala.slick.jdbc.StaticQuery
    case class Coffee(id: Int, name: String)
  }
}