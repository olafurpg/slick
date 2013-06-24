package scala.slick.yy

import ch.epfl.yinyang.api.{ BaseYinYang, Interpreted, FullyStaged }
import scala.slick.yy.{ Shallow => OShallow }
import scala.reflect.runtime.{ universe => ru }
import scala.slick.ast.QueryParameter
import scala.slick.jdbc.UnitInvoker

trait SlickYinYang extends SlickConstYinYang with YYSlickCake with Interpreted with FullyStaged {
  //  def stagingAnalyze(allHoles: List[scala.Int]): List[scala.Int] = allHoles
  def reset() = {
    alreadyInterpreted = false
  }
  var alreadyInterpreted: scala.Boolean = false
  var cachedInvoker: UnitInvoker[_] = _
  var previousResult: Any = _
  def interpret[T: ru.TypeTag](params: Any*): T = {
    @inline def handleResult(result: Any): Any = {
      result match {
        case yyQuery: YYQuery[_] => new TransferQuery(result.asInstanceOf[YYQuery[_]])
        case inv @ YYInvoker(q, t, driver, s) => {
          val invoker = {
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
}

trait TransferCake { self: YYSlickCake =>
  class TransferQuery[T](val underlying: YYQuery[T]) extends OShallow.Query[T]
}

trait SlickConstYinYang extends scala.slick.driver.JdbcDriver.ImplicitJdbcTypes with BaseYinYang with TransferCake { self: YYSlickCake =>
  import scala.slick.ast.TypedType
  implicit object LiftUnit extends LiftEvidence[Unit, Unit] {
    def lift(v: Unit): Unit = v
    def hole(tpe: ru.TypeTag[Unit], symbolId: scala.Int): Unit = ()
  }
  implicit def LiftConst[T, S](implicit cstTpe: YYConstantType[T, S], ttag: ru.TypeTag[T], tpe: TypedType[T]): LiftEvidence[T, S] = new LiftEvidence[T, S] {
    def lift(v: T): S = YYConstColumn(v).asInstanceOf[S]
    def hole(tptag: ru.TypeTag[T], symbolId: scala.Int): S = ???
  }
  implicit def liftQuery[T](implicit ttag: ru.TypeTag[OShallow.Query[T]]): LiftEvidence[OShallow.Query[T], Query[T]] = new LiftEvidence[OShallow.Query[T], Query[T]] {
    def lift(v: OShallow.Query[T]): Query[T] = v.asInstanceOf[TransferQuery[T]].underlying
    def hole(tpe: ru.TypeTag[OShallow.Query[T]], symbolId: scala.Int): Query[T] = ???
  }
}
