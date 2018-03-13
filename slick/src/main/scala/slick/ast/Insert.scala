package slick.ast

import slick.util.ConstArray
import java.lang
import scala.collection.immutable
import slick.ast.{ Insert, InsertColumn, Node, TermSymbol }
import slick.util.DumpInfo

/** Represents an Insert operation. */
final case class Insert(tableSym: TermSymbol, table: Node, linear: Node, allFields: ConstArray[FieldSymbol]) extends BinaryNode with DefNode {
  type Self = Insert
  def left: Node = table
  def right: Node = linear
  override def childNames: immutable.Vector[lang.String] = Vector("table "+tableSym, "linear")
  def generators: ConstArray[(TermSymbol, Node)] = ConstArray((tableSym, table))
  def rebuild(l: Node, r: Node): Insert = copy(table = l, linear = r)
  def rebuildWithSymbols(gen: ConstArray[TermSymbol]): Insert = copy(tableSym = gen(0))
  def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self = {
    val table2 = table.infer(scope, typeChildren)
    val lin2 = linear.infer(scope + (tableSym -> table2.nodeType), typeChildren)
    withChildren(ConstArray[Node](table2, lin2)) :@ (if(!hasType) lin2.nodeType else nodeType)
  }
  override def getDumpInfo: DumpInfo = super.getDumpInfo.copy(mainInfo = allFields.mkString("allFields=[", ", ", "]"))
}

/** A column in an Insert operation. */
final case class InsertColumn(children: ConstArray[Node], fs: FieldSymbol, buildType: Type) extends Node with SimplyTypedNode {
  type Self = InsertColumn
  protected[this] def rebuild(ch: ConstArray[Node]): InsertColumn = copy(children = ch)
  override def getDumpInfo: DumpInfo = super.getDumpInfo.copy(mainInfo = fs.toString)
}
