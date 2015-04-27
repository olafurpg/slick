package scala.slick.typeproviders.codegenerator

import scala.reflect.runtime.universe._

trait ExpressionComponent { self: Generator =>
  def generateCodeForTree(tree: Tree): String =
    generateCodeForTree(tree, None)

  def generateCodeForTree(tree: Tree, parent: Option[Tree]): String = {
    @inline def rec(t: Tree): String = generateCodeForTree(t, Some(tree))
    tree match {
      case Apply(Select(qualifier, name), args) if parent.collect { case a @ Apply(_, _) => a }.isEmpty && Some(qualifier).collect { case n @ New(_) => n }.isEmpty => {
        val operand = generateCodeForTree(qualifier)
        val op = generateCodeForName(name)
        val arg = args match {
          case List(elem) => rec(elem)
          case list => list.map(rec).mkString("(", ", ", ")")
        }
        s"$operand $op $arg"
      }
      case Apply(fun, args) =>
        s"${rec(fun)}(${args.map(rec).mkString(", ")})"
      case Select(New(qualifier), name) =>
        s"new ${rec(qualifier)}${generateCodeForName(name)}"
      case Select(qualifier, name) =>
        s"${rec(qualifier)}.${generateCodeForName(name)}"
      case Ident(ident) =>
        generateCodeForName(ident)
      case Function(List(ValDef(_, TermName(variable), _, _)), body) =>
        // not complete. doesn't the case of having more than input
        s"$variable => (${rec(body)})"
      case _ =>
        tree.toString
    }
  }

  def generateCodeForName(name: String): String = {
    name match {
      case "$times" => "*"
      case "$less$greater" => "<>"
      case "$tilde" => "~"
      case _ => name
    }
  }

  def generateCodeForName(name: Name): String = {
    if (name equals nme.CONSTRUCTOR)
      ""
    else
      generateCodeForName(name.toString)
  }

  def generateCodeForModifiers(mods: Modifiers): String = {
    val flags = Map(Flag.ABSTRACT -> "abstract",
      Flag.OVERRIDE -> "override",
      Flag.IMPLICIT -> "implicit",
      Flag.PRIVATE -> "private"
    )
    flags.filter(x => mods.hasFlag(x._1)).map(_._2) match {
      case Nil => ""
      case list => list.mkString("", " ", " ")
    }
  }
}