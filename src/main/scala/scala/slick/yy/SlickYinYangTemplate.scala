package scala.slick.yy

import ch.epfl.yinyang.api.{ Interpreted, FullyUnstaged, BaseYinYang, HoleTypeAnalyser }
import scala.slick.yy.{ Shallow => OShallow }
import scala.reflect.runtime.{ universe => ru }
import scala.slick.jdbc.{ JdbcBackend }
import scala.slick.driver.{ JdbcProfile }
import scala.slick.lifted.{ Query }
import scala.slick.jdbc.{ UnitInvoker }
import scala.slick.lifted.NothingContainer
import scala.slick.SlickException

trait SlickYinYangTemplate extends SlickConstYinYangTemplate with YYSlickCake with Interpreted with HoleTypeAnalyser {
  //  def stagingAnalyze(allHoles: List[scala.Int]): List[scala.Int] = allHoles
  //  override def requiredHoles(allCaptures: List[ru.Symbol]): List[scala.Int] = {
  //    allCaptures foreach { v =>
  //      println(v.typeSignature)
  //    }
  //    allCaptures map (_.asInstanceOf[scala.reflect.internal.Symbols#Symbol].id)
  //  }
  @volatile private var alreadyInterpreted: scala.Boolean = false
  @volatile private var isCached: scala.Boolean = false
  @volatile private var cachedDriver: JdbcProfile = _
  var previousResult: Any = _
  var cachedInvoker: UnitInvoker[_] = _
  var cachedTemplate: JdbcProfile#QueryTemplate[Any, Any] = _
  var cachedInsertInvoker: JdbcProfile#CountingInsertInvoker[Any] = _
  var cachedUpdateInvoker: JdbcProfile#UpdateInvoker[Any] = _
  var cachedUpdateTemplateInvoker: JdbcProfile#UpdateTemplateInvoker[Any] = _
  var cachedConstCaseClass: Any = _

  def reset() {
    alreadyInterpreted = false
  }

  def checkAndFillCaches(params: IndexedSeq[Any], query: YYQuery[Any], driver: JdbcProfile) {
    if (!isCached || cachedDriver != driver) {
      fillCaches(params, query, driver)
      cachedDriver = driver
      isCached = true
    }
  }

  def fillCaches(params: IndexedSeq[Any], query: YYQuery[Any], driver: JdbcProfile) {
    if (params.length > 0)
      cachedTemplate = driver.queryToQueryTemplate[Any, Any](query.query)
    else {
      cachedInvoker = driver.Implicit.queryToQueryInvoker(query.query)
    }
    try {
      cachedInsertInvoker = driver.Implicit.columnBaseToInsertInvoker(query.repValue.asInstanceOf[scala.slick.lifted.ColumnBase[Any]])
    } catch {
      case se: SlickException =>
    }
    try {
      val compiledTree = driver.updateStatementCompiler.run(scala.slick.ast.Node(query.query)).tree
      cachedUpdateTemplateInvoker = new driver.UpdateTemplateInvoker(compiledTree)
    } catch {
      case se: SlickException =>
    }
  }

  def interpret[T: ru.TypeTag](params: Any*): T = {
    @inline def getValue(value: YYValue[_]): Any = value match {
      case caseRep @ YYCaseRep(const, fields) =>
        caseRep.getValue(params.toIndexedSeq)
      case caseRep @ YYConstCaseRep(const, fields) =>
        if (!alreadyInterpreted) {
          cachedConstCaseClass = caseRep.getValue
        }
        cachedConstCaseClass
      case _ =>
        value
    }
    @inline def handleResult(result: Any): Any = {
      result match {
        case yyQuery: YYQuery[_] => new TransferQuery(result.asInstanceOf[YYQuery[_]], this, params.toIndexedSeq)
        //        case yyQT: YYQueryTemplate[_] => {
        //          import yyQT._
        //          val invoker = qt.apply(params(0))
        //          if (isFirst)
        //            invoker.first()(session)
        //          else
        //            invoker.list()(session)
        //        }
        //        //        case yyQTW: YYQueryTemplateWrapper[_, _] => new TransferQueryTemplate(yyQTW)
        //        case YYQueryExecuter(query, driver) => {
        //          val qe = new QueryExecutor[T](this)
        //          qe.params = params.toIndexedSeq
        //          checkAndFillCaches(qe.params, query, driver)
        //          qe
        //        }
        //        case inv @ YYInvoker(q: YYQuery[_], _, driver, _) => {
        //          val invoker =
        //            // Query Template!
        //            if (params.length > 0) {
        //              if (!alreadyInterpreted) {
        //                cachedTemplate = driver.queryToQueryTemplate[Any, Any](q.query)
        //                val compiledTree = driver.selectStatementCompiler.run(scala.slick.ast.Node(q.query)).tree
        //              }
        //              if (params.length == 1)
        //                cachedTemplate(params.toIndexedSeq)
        //              else
        //                cachedTemplate(params.toIndexedSeq)
        //            } // Query
        //            else {
        //              if (!alreadyInterpreted) {
        //                cachedInvoker = driver.Implicit.queryToQueryInvoker(q.query)
        //              }
        //              cachedInvoker
        //            }
        //          inv.invoke(Some(invoker))
        //        }
        //        case YYInsertInvoker(query, value: YYValue[_], driver, session) => {
        //          val invoker = {
        //            if (!alreadyInterpreted) {
        //              cachedInsertInvoker = driver.Implicit.columnBaseToInsertInvoker(query.repValue.asInstanceOf[scala.slick.lifted.ColumnBase[Any]])
        //            }
        //            cachedInsertInvoker
        //          }
        //          invoker.insert(getValue(value))(session)
        //        }
        //        case YYUpdateInvoker(query, value: YYValue[_], driver, session) => {
        //          val invoker = {
        //            if (!alreadyInterpreted) {
        //              val compiledTree = driver.updateStatementCompiler.run(scala.slick.ast.Node(query.query)).tree
        //              // cachedUpdateInvoker = driver.Implicit.tableQueryToUpdateInvoker(query.query.asInstanceOf[scala.slick.lifted.Query[driver.Table[Any], NothingContainer#TableNothing]])
        //              cachedUpdateTemplateInvoker = new driver.UpdateTemplateInvoker(compiledTree)
        //              // pw.println(scala.slick.ast.Dump.get(compiledTree))
        //              // pw.println(cachedUpdateTemplateInvoker.updateStatement)
        //              // pw.println(s"driver: $driver")
        //              // pw.flush()
        //              // // pw.println(cachedUpdateInvoker.updateStatement)
        //              // // pw.flush()
        //            }
        //            cachedUpdateTemplateInvoker
        //            // cachedUpdateInvoker
        //          }
        //          // pw.println(s"params: $params")
        //          // pw.println(s"value: ${getValue(value)}")
        //          // pw.println(s"session: $session")
        //          // pw.flush()
        //          val newParams =
        //            if (params.length == 0)
        //              null
        //            else if (params.length == 1)
        //              params.toIndexedSeq
        //            else
        //              params.toIndexedSeq
        //          invoker.update(getValue(value))(newParams)(session)
        //          // invoker.update(getValue(value))(session)
        //        }
        case value: YYValue[_] =>
          getValue(value)
        case _ => result
      }
    }
    if (!alreadyInterpreted) {
      val result = main()
      val newRes = handleResult(result)
      alreadyInterpreted = true
      previousResult = result
      newRes.asInstanceOf[T]
    } else {
      val newRes = handleResult(previousResult)
      newRes.asInstanceOf[T]
    }
  }
  def main(): Any

  type Seq[T] = CakeRep[scala.Seq[T]]
}

//trait YYQueryTemplateComponent[U] { this: YYQuery[U] =>
//  def firstTemplate(implicit driver: JdbcProfile, session: JdbcBackend#Session): YYQueryTemplate[U] = new YYQueryTemplate(driver.queryToQueryTemplate[Any, Any](this.query.asInstanceOf[Query[Any, Any]]), true, driver, session)
//  def toSeqTemplate(implicit driver: JdbcProfile, session: JdbcBackend#Session): YYQueryTemplate[U] = new YYQueryTemplate(driver.queryToQueryTemplate[Any, Any](this.query.asInstanceOf[Query[Any, Any]]), false, driver, session)
//  def getQueryTemplate[P](implicit driver: JdbcProfile): YYQueryTemplateWrapper[P, U] = YYQueryTemplateWrapper(driver.queryToQueryTemplate[P, U](this.query))
//  def funcTemplate(implicit driver: JdbcProfile): YYQueryTemplateWrapper[Any, U] = YYQueryTemplateWrapper(driver.queryToQueryTemplate[Any, U](this.query))
//}
//
//class YYQueryTemplate[U](val qt: JdbcProfile#QueryTemplate[Any, Any], val isFirst: Boolean, val driver: JdbcProfile, val session: JdbcBackend#Session)
//
//class YYQueryTemplateRep[U](qt: JdbcProfile#QueryTemplate[Any, Any], isFirst: Boolean, driver: JdbcProfile, session: JdbcBackend#Session) extends YYQueryTemplate[U](qt, isFirst, driver, session) with YYRep[Seq[U]] /* A hack for type checking */ {
//  def underlying = ???
//}
//
//case class YYInvoker[T](queryOrTemplate: YYQuery[T], kind: YYInvoker.InvokerType, driver: JdbcProfile, session: JdbcBackend#Session) {
//  import YYInvoker._
//  def invoke(inv: Option[UnitInvoker[T]] = None): Any = {
//    val invoker = inv getOrElse {
//      queryOrTemplate match {
//        case yyQuery: YYQuery[_] => yyQuery.invoker(driver)
//      }
//    }
//    kind match {
//      case List => invoker.list()(session)
//      case First => invoker.first()(session)
//    }
//  }
//}

//class QueryExecutor[T](cache: SlickYinYangTemplate) {
//  import cache.{ cachedInsertInvoker, cachedInvoker, cachedTemplate, cachedUpdateTemplateInvoker }
//  var params: IndexedSeq[Any] = _
//
//  def list()(implicit session: JdbcBackend#Session): List[T] = {
//    if (params.length == 0)
//      cachedInvoker.list().asInstanceOf[List[T]]
//    else
//      cachedTemplate(params).list().asInstanceOf[List[T]]
//  }
//  def first()(implicit session: JdbcBackend#Session): T = {
//    if (params.length == 0)
//      cachedInvoker.first().asInstanceOf[T]
//    else
//      cachedTemplate(params).first().asInstanceOf[T]
//  }
//  def insert(value: T)(implicit session: JdbcBackend#Session): Int = {
//    cachedInsertInvoker.insert(value)
//  }
//  def update(value: T)(implicit session: JdbcBackend#Session): Int = {
//    cachedUpdateTemplateInvoker.update(value)(params)
//  }
//}

//case class YYQueryExecuter[T](query: YYQuery[T], driver: JdbcProfile)

class ShadowExecutor[T](val query: Shallow.Query[T]) {
  val transferQuery = query.asInstanceOf[TransferQuery[T]]
  import transferQuery.{ underlying => deepQuery, cake, params }

  import cake.{ cachedInsertInvoker, cachedInvoker, cachedTemplate, cachedUpdateTemplateInvoker }

  def checkCache(implicit driver: JdbcProfile) {
    cake.checkAndFillCaches(params, deepQuery.asInstanceOf[YYQuery[Any]], driver)
  }

  def list()(implicit driver: JdbcProfile, session: JdbcBackend#Session): List[T] = {
    checkCache
    if (params.length == 0)
      cachedInvoker.list().asInstanceOf[List[T]]
    else
      cachedTemplate(params).list().asInstanceOf[List[T]]
  }
  def first()(implicit driver: JdbcProfile, session: JdbcBackend#Session): T = {
    checkCache
    if (params.length == 0)
      cachedInvoker.first().asInstanceOf[T]
    else
      cachedTemplate(params).first().asInstanceOf[T]
  }
  def insert(value: T)(implicit driver: JdbcProfile, session: JdbcBackend#Session): Int = {
    checkCache
    cachedInsertInvoker.insert(value)
  }
  def update(value: T)(implicit driver: JdbcProfile, session: JdbcBackend#Session): Int = {
    checkCache
    cachedUpdateTemplateInvoker.update(value)(params)
  }
}

//object YYInvoker {
//  sealed trait InvokerType
//  case object List extends InvokerType
//  case object First extends InvokerType
//}
//
//case class YYQueryTemplateWrapper[P, R](val underlying: JdbcProfile#QueryTemplate[P, R]) {
//  def apply(param: YYColumn[P])(implicit driver: JdbcProfile, session: JdbcBackend#Session): YYQueryTemplateRep[R] = new YYQueryTemplateRep[R](underlying.asInstanceOf[driver.QueryTemplate[Any, Any]], false, driver, session)
//  //  def apply(param: YYColumn[P])(implicit driver: JdbcProfile, session: JdbcBackend#Session): YYRep[R] = YYConstColumn(underlying(param.getValue).first())(null)
//}

trait YYHole {
  val index: Int
}

object YYHole {
  import scala.slick.ast.TypedType
  import scala.slick.lifted.{ Column }
  import scala.slick.ast.{ QueryParameter }
  def apply[T](index: Int)(implicit tpe: TypedType[T]): YYHole = {
    def extractor(x: Any): Any = {
      val res = x match {
        case seq: IndexedSeq[_] => seq(index)
        case _ => x
      }
      res
    }
    val c = Column.forNode[T](new QueryParameter(extractor, tpe))(tpe)
    val i = index
    new YYColumn[T] with YYHole {
      val column = c
      val index = i
    }
  }
}

trait SlickConstYinYangTemplate extends scala.slick.driver.JdbcDriver.ImplicitJdbcTypes with BaseYinYang { self: YYSlickCake =>
  import scala.slick.ast.TypedType
  implicit object LiftUnit extends LiftEvidence[Unit, Unit] {
    def lift(v: Unit): Unit = v
    def hole(tpe: ru.TypeTag[Unit], symbolId: scala.Int): Unit = ()
  }
  implicit def LiftConst[T, S](implicit cstTpe: YYConstantType[T, S], ttag: ru.TypeTag[T], tpe: TypedType[T]): LiftEvidence[T, S] = new LiftEvidence[T, S] {
    def lift(v: T): S = YYConstColumn(v).asInstanceOf[S]
    def hole(tptag: ru.TypeTag[T], symbolId: scala.Int): S = {
      YYHole[T](symbolId).asInstanceOf[S]
    }
  }
  implicit def liftQuery[T](implicit ttag: ru.TypeTag[OShallow.Query[T]]): LiftEvidence[OShallow.Query[T], Query[T]] = new LiftEvidence[OShallow.Query[T], Query[T]] {
    def lift(v: OShallow.Query[T]): Query[T] = v.asInstanceOf[TransferQuery[T]].underlying
    def hole(tpe: ru.TypeTag[OShallow.Query[T]], symbolId: scala.Int): Query[T] = ???
  }
}
