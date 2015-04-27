package scala.slick.typeproviders.codegenerator

trait Generator extends TemplateComponent
  with DefComponent with ExpressionComponent with TypeComponent {
  import scala.reflect.runtime.{ universe => runtimeUniverse }
  import runtimeUniverse._

  object TermName {
    def apply(s: String): runtimeUniverse.TermName = runtimeUniverse.newTermName(s)
    def unapply(t: runtimeUniverse.TermName): Option[String] = Some(t.toString)
  }
  object TypeName {
    def apply(s: String): runtimeUniverse.TypeName = runtimeUniverse.newTypeName(s)
    def unapply(t: runtimeUniverse.TypeName): Option[String] = Some(t.toString)
  }

  private var indentLevel = 0

  def incIndent {
    indentLevel += 1
  }

  def decIndent {
    indentLevel -= 1
  }

  def genIndent: String = {
    val result = new StringBuffer
    for (i <- 1 to indentLevel) {
      result append "  "
    }
    result.toString
  }

  def generateCodeForTable(tree: Tree): String = {
    tree match {
      case classDef: ClassDef =>
        // we're sure that it would be the generated case class for it
        generateCodeForClass(classDef)
      case moduleDef: ModuleDef =>
        generateCodeForModule(moduleDef)
      case typeDef: TypeDef =>
        generateCodeForTypeDef(typeDef)
      case valDef: ValDef =>
        generateCodeForValDef(valDef)
      case defDef: DefDef =>
        generateCodeForDefDef(defDef)
      case _ => tree.toString
    }
  }

  def generateCodeForImport(imp: Import): String = {
    s"$genIndent$imp"
  }
}