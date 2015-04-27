package scala.slick.typeproviders.codegenerator

import scala.reflect.runtime.universe._

trait DefComponent { self: Generator =>
  def generateCodeForValDef(valDef: ValDef, withVal: Boolean = true, withIndent: Boolean = true): String = {
    val ValDef(mods, TermName(fieldName), tpt, rhs) = valDef
    val typeCode = if (tpt.isEmpty) "" else s": ${generateCodeForTypeTree(tpt)}"
    val fieldNameCode = generateCodeForName(fieldName)
    val valCode = if (withVal) "val " else ""
    val indent = if (withIndent) genIndent else ""
    s"$indent${generateCodeForModifiers(mods)}$valCode$fieldNameCode$typeCode" + {
      if (rhs.isEmpty) ""
      else " = " + generateCodeForTree(rhs)
    }
  }

  def generateCodeForDefDef(defDef: DefDef): String = {
    val DefDef(mods, TermName(methodName), tparams, vparamss, tpt, rhs) = defDef
    val methodNameCode = generateCodeForName(methodName)
    val typeCode = if (tpt.isEmpty) "" else s": ${generateCodeForTypeTree(tpt)}"
    val tParamsCode = "" // TODO not handled!
    val vParamssCode = vparamss match {
      case Nil => ""
      case _ => vparamss map {
        _ map { param =>
          generateCodeForValDef(param, false, false)
        } mkString ("(", ", ", ")")

      } mkString ("")
    }
    s"${genIndent}${generateCodeForModifiers(mods)}def $methodNameCode$tParamsCode$vParamssCode$typeCode" + {
      if (rhs.isEmpty) ""
      else " = " + generateCodeForTree(rhs)
    }
  }

}