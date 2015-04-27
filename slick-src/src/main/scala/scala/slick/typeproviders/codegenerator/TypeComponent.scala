package scala.slick.typeproviders.codegenerator

import scala.reflect.runtime.universe._

trait TypeComponent { self: Generator =>
  def generateCodeForTypeTree(tpt: Tree): String = {
    tpt match {
      case typeTree: TypeTree if typeTree.original != null => typeTree.original.toString
      case _ => tpt.toString
    }
  }

  def generateCodeForTypeDef(typeDef: TypeDef): String = {
    val TypeDef(mods, TypeName(typeName), List(), typeType) = typeDef
    s"${genIndent}${generateCodeForModifiers(mods)}type $typeName = ${generateCodeForTypeTree(typeType)}"
  }
}