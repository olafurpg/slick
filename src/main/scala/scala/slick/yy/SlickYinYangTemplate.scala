package scala.slick.yy

import ch.epfl.yinyang.api.{ Interpreted, FullyUnstaged, BaseYinYang, HoleTypeAnalyser }
import scala.slick.yy.{ Shallow => OShallow }
import scala.reflect.runtime.{ universe => ru }
import scala.slick.jdbc.{ JdbcBackend }
import scala.slick.driver.{ JdbcProfile }
import scala.slick.lifted.{ Query }
import scala.slick.jdbc.UnitInvoker

trait SlickYinYangTemplate extends SlickConstYinYangTemplate with YYSlickCake with Interpreted with HoleTypeAnalyser {
  //  def stagingAnalyze(allHoles: List[scala.Int]): List[scala.Int] = allHoles
  //  override def requiredHoles(allCaptures: List[ru.Symbol]): List[scala.Int] = {
  //    allCaptures foreach { v =>
  //      println(v.typeSignature)
  //    }
  //    allCaptures map (_.asInstanceOf[scala.reflect.internal.Symbols#Symbol].id)
  //  }
  var alreadyInterpreted: scala.Boolean = false
  var previousResult: Any = _
  var cachedInvoker: UnitInvoker[_] = _
  var cachedTemplate: JdbcProfile#QueryTemplate[Any, Any] = _

  def reset() {
    alreadyInterpreted = false
  }
  def interpret[T: ru.TypeTag](params: Any*): T = {
    @inline def handleResult(result: Any): Any = {
      result match {
        case yyQuery: YYQuery[_] => new TransferQuery(result.asInstanceOf[YYQuery[_]])
        case yyQT: YYQueryTemplate[_] => {
          import yyQT._
          val invoker = qt.apply(params(0))
          if (isFirst)
            invoker.first()(session)
          else
            invoker.list()(session)
        }
        case yyQTW: YYQueryTemplateWrapper[_, _] => new TransferQueryTemplate(yyQTW)
        case inv @ YYInvoker(q: YYQuery[_], _, driver, _) => {
          val invoker =
            // Query Template!
            if (params.length > 0) {
              if (!alreadyInterpreted) {
                cachedTemplate = driver.queryToQueryTemplate[Any, Any](q.query)
              }
              cachedTemplate(params(0))
            } // Query
            else {
              if (!alreadyInterpreted) {
                cachedInvoker = driver.Implicit.queryToQueryInvoker(q.query)
              }
              cachedInvoker
            }
          inv.invoke(Some(invoker))
        }
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

trait YYQueryTemplateComponent[U] { this: YYQuery[U] =>
  def firstTemplate(implicit driver: JdbcProfile, session: JdbcBackend#Session): YYQueryTemplate[U] = new YYQueryTemplate(driver.queryToQueryTemplate[Any, Any](this.query.asInstanceOf[Query[Any, Any]]), true, driver, session)
  def toSeqTemplate(implicit driver: JdbcProfile, session: JdbcBackend#Session): YYQueryTemplate[U] = new YYQueryTemplate(driver.queryToQueryTemplate[Any, Any](this.query.asInstanceOf[Query[Any, Any]]), false, driver, session)
  def getQueryTemplate[P](implicit driver: JdbcProfile): YYQueryTemplateWrapper[P, U] = YYQueryTemplateWrapper(driver.queryToQueryTemplate[P, U](this.query))
  def funcTemplate(implicit driver: JdbcProfile): YYQueryTemplateWrapper[Any, U] = YYQueryTemplateWrapper(driver.queryToQueryTemplate[Any, U](this.query))
}

class YYQueryTemplate[U](val qt: JdbcProfile#QueryTemplate[Any, Any], val isFirst: Boolean, val driver: JdbcProfile, val session: JdbcBackend#Session)

// A hack for type checking
class YYQueryTemplateRep[U](qt: JdbcProfile#QueryTemplate[Any, Any], isFirst: Boolean, driver: JdbcProfile, session: JdbcBackend#Session) extends YYQueryTemplate[U](qt, isFirst, driver, session) with YYRep[Seq[U]] {
  def underlying = ???
}

case class YYInvoker[T](queryOrTemplate: YYQuery[T], kind: YYInvoker.InvokerType, driver: JdbcProfile, session: JdbcBackend#Session) {
  import YYInvoker._
  def invoke(inv: Option[UnitInvoker[T]] = None): Any = {
    val invoker = inv getOrElse {
      queryOrTemplate match {
        case yyQuery: YYQuery[_] => yyQuery.invoker(driver)
      }
    }
    kind match {
      case List => invoker.list()(session)
      case First => invoker.first()(session)
    }
  }
}

object YYInvoker {
  sealed trait InvokerType
  case object List extends InvokerType
  case object First extends InvokerType
}

case class YYQueryTemplateWrapper[P, R](val underlying: JdbcProfile#QueryTemplate[P, R]) {
  def apply(param: YYColumn[P])(implicit driver: JdbcProfile, session: JdbcBackend#Session): YYQueryTemplateRep[R] = new YYQueryTemplateRep[R](underlying.asInstanceOf[driver.QueryTemplate[Any, Any]], false, driver, session)
  //  def apply(param: YYColumn[P])(implicit driver: JdbcProfile, session: JdbcBackend#Session): YYRep[R] = YYConstColumn(underlying(param.getValue).first())(null)
}

trait TransferCakeTemplate { self: YYSlickCake =>
  class TransferQueryTemplate[P, R](val underlying: YYQueryTemplateWrapper[P, R]) extends OShallow.QueryTemplate[P, R]
}

trait SlickConstYinYangTemplate extends scala.slick.driver.JdbcDriver.ImplicitJdbcTypes with BaseYinYang with TransferCake with TransferCakeTemplate { self: YYSlickCake =>
  import scala.slick.ast.TypedType
  implicit object LiftUnit extends LiftEvidence[Unit, Unit] {
    def lift(v: Unit): Unit = v
    def hole(tpe: ru.TypeTag[Unit], symbolId: scala.Int): Unit = ()
  }
  implicit def LiftConst[T, S](implicit cstTpe: YYConstantType[T, S], ttag: ru.TypeTag[T], tpe: TypedType[T]): LiftEvidence[T, S] = new LiftEvidence[T, S] {
    def lift(v: T): S = YYConstColumn(v).asInstanceOf[S]
    def hole(tptag: ru.TypeTag[T], symbolId: scala.Int): S = {
      import scala.slick.lifted.{ Column }
      import scala.slick.ast.{ QueryParameter }
      YYColumn(Column.forNode[T](new QueryParameter((x: Any) => x, tpe))(tpe)).asInstanceOf[S]
    }
  }
  implicit def liftQuery[T](implicit ttag: ru.TypeTag[OShallow.Query[T]]): LiftEvidence[OShallow.Query[T], Query[T]] = new LiftEvidence[OShallow.Query[T], Query[T]] {
    def lift(v: OShallow.Query[T]): Query[T] = v.asInstanceOf[TransferQuery[T]].underlying
    def hole(tpe: ru.TypeTag[OShallow.Query[T]], symbolId: scala.Int): Query[T] = ???
  }
  implicit def liftQueryTemplate[P, R](implicit ttag: ru.TypeTag[OShallow.QueryTemplate[P, R]]): LiftEvidence[OShallow.QueryTemplate[P, R], YYQueryTemplateWrapper[P, R]] = new LiftEvidence[OShallow.QueryTemplate[P, R], YYQueryTemplateWrapper[P, R]] {
    def lift(v: OShallow.QueryTemplate[P, R]): YYQueryTemplateWrapper[P, R] = v.asInstanceOf[TransferQueryTemplate[P, R]].underlying
    def hole(tpe: ru.TypeTag[OShallow.QueryTemplate[P, R]], symbolId: scala.Int): YYQueryTemplateWrapper[P, R] = ???
  }
}
