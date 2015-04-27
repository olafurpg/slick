package scala.slick.typeproviders.codegenerator

import scala.reflect.runtime.universe._
import Flag._
import scala.slick.SlickException

trait TemplateComponent { self: Generator =>
  def generateCodeForClass(classDef: ClassDef): String = {
    val isCaseClass = classDef.mods.hasFlag(CASE)
    val ClassDef(mods, TypeName(name), _, template @ Template(_, _, fieldsCtor)) = classDef
    if (!isCaseClass) {
      generateCodeForTemplate(mods, true, name, template)
    } else {
      // We don't need constructor of case class, we're only interested in
      // fields. Also we don't consider the methods which are implemented for
      // that case class
      val fields = fieldsCtor collect {
        //        case ValDef(_, TermName(fieldName), tpe, _) => s"$fieldName: ${generateCodeForTypeTree(tpe)}"
        case valDef: ValDef => generateCodeForValDef(valDef, true, false)
      }
      s"${genIndent}case class $name(${fields.mkString(", ")})"
    }
  }

  def generateCodeForModule(moduleDef: ModuleDef): String = {
    val ModuleDef(mods, TermName(name), template) = moduleDef
    generateCodeForTemplate(mods, false, name, template)
  }

  def generateCodeForTemplate(mods: Modifiers, isClass: Boolean, name: String, template: Template): String = {
    val Template(List(tableSuper), self, methods) = template
    val tableSuperCode = generateCodeForTypeTree(tableSuper)
    val fieldNames = methods collect {
      case ValDef(_, TermName(fieldName), _, _) => fieldName
    }

    val (constructor, ctorParams, superConstructor) = methods collectFirst {
      case DefDef(_, methodName, _, vparamss, _, Block(List(Apply(_, ctorArgs)), _)) if methodName == nme.CONSTRUCTOR => {
        val (constructor, ctorParams) = vparamss match {
          case Nil => ("", Nil)
          case List(Nil) => ("", Nil)
          // TODO each parameter should be checked to see it's in valdef of template body
          case List(vparam) => (vparam.map(valDef => generateCodeForValDef(valDef, fieldNames.exists(_.equals(valDef.name.decoded)), false)).mkString("(", ", ", ")"), vparam)
        }
        (constructor, ctorParams, ctorArgs.map(generateCodeForTree).mkString("(", ", ", ")"))
      }
    } getOrElse ("", Nil, "")
    incIndent
    val methodsCode = methods collect {
      case method @ DefDef(_, methodName, _, _, _, rhs) if methodName != nme.CONSTRUCTOR => {
        generateCodeForDefDef(method)
      }
      case valDef @ ValDef(_, TermName(fieldName), _, _) if !ctorParams.exists(_.name.decoded.equals(fieldName)) => {
        generateCodeForValDef(valDef)
      }
    }
    val classOrObject = if (isClass) "class" else "object"
    decIndent
    s"""${genIndent}${generateCodeForModifiers(mods)}$classOrObject $name$constructor extends $tableSuperCode $superConstructor{
${methodsCode.mkString("\n")}
${genIndent}}"""
  }

}