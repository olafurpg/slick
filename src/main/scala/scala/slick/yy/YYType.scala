package scala.slick.yy

trait YYType {
  type CakeRep[T] = YYRep[T]
  type Column[T] = YYColumn[T]
  type Table[T] = YYTable[T]
  type Query[T] = YYQuery[T]
  type JoinQuery[T1, T2] = YYJoinQuery[T1, T2]
  type Int = YYColumn[scala.Int]
  type Long = YYColumn[scala.Long]
  type Double = YYColumn[scala.Double]
  type String = YYColumn[Predef.String]
  type Boolean = YYColumn[scala.Boolean]
  type ColumnOps[T] = YYColumn[T]
//  type Invoker[T] = Shallow.Invoker[T]
  type Ordering[T] = YYOrdering[T]
  type Option[T] = YYColumn[scala.Option[T]]
  type SingleColumnQuery[T] = YYSingleColumnQuery[T]
  class OptMaker[T](val value: YYColumn[T]) {
    def ? : YYOption[T] = YYOption.fromPlainColumn(value.underlying)
  }
}

trait YYConstantSourceType[T]
trait YYConstantType[T, S] extends YYConstantSourceType[T]

object YYConstantType extends YYType {
  implicit object IntType extends YYConstantType[scala.Int, Int]
  implicit object LongType extends YYConstantType[scala.Long, Long]
  implicit object DoubleType extends YYConstantType[scala.Double, Double]
  implicit object BooleanType extends YYConstantType[scala.Boolean, Boolean]
  implicit object StringType extends YYConstantType[Predef.String, String]
}