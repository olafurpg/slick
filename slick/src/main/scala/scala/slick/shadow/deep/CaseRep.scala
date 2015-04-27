package scala.slick.shadow.deep

case class CaseRep[T](const: (Product => T), fields: IndexedSeq[YYColumn[_]]) extends YYRep[T] with YYValue[T] {
  def underlying = ???
  def getValue: T = const.apply(scala.slick.util.TupleSupport.buildTuple(fields map (_.getValue)))
  def getValue(params: IndexedSeq[Any]): T = const.apply(scala.slick.util.TupleSupport.buildTuple({
    fields map {
      case hole: Hole =>
        params(hole.index)
      case x => x.getValue
    }
  }
  ))
}
case class ConstCaseRep[T](const: (Product => T), fieldValues: IndexedSeq[_]) extends YYRep[T] with YYValue[T] {
  def underlying = ???
  def getValue: T = const.apply(scala.slick.util.TupleSupport.buildTuple(fieldValues))
  def getValue(params: IndexedSeq[Any]): T = getValue
}
object ThisCase {
  class ThisCaseWithConstructor[T](val const: (Product => T)) {
    def apply(fields: YYColumn[_]*): YYRep[T] = {
      if (fields.exists(_.isInstanceOf[Hole]))
        CaseRep(const, fields.toIndexedSeq)
      else
        ConstCaseRep(const, fields.toIndexedSeq map (_.getValue))
    }
  }
  def apply[T](const: (Product => T)): ThisCaseWithConstructor[T] = new ThisCaseWithConstructor(const)
}