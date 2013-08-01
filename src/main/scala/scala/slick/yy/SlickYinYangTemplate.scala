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
  def foreach(f: T => Unit)(implicit driver: JdbcProfile, session: JdbcBackend#Session) {
    list() foreach (f)
  }
}

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
