package scala.slick.shadow.deep

trait Hole {
  val index: Int
}

object Hole {
  import scala.slick.ast.TypedType
  import scala.slick.lifted.{ Column }
  import scala.slick.ast.{ QueryParameter }
  def apply[T](index: Int)(implicit tpe: TypedType[T]): Hole = {
    def extractor(x: Any): Any = {
      val res = x match {
        case seq: IndexedSeq[_] => seq(index)
        case _ => x
      }
      res
    }
    val c = Column.forNode[T](new QueryParameter(extractor, tpe))(tpe)
    val i = index
    new YYColumn[T] with Hole {
      val column = c
      val index = i
    }
  }
}