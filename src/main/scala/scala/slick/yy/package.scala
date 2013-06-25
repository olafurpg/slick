package scala.slick

import ch.epfl.yinyang._
import ch.epfl.yinyang.typetransformers.PolyTransformer
import ch.epfl.yinyang.transformers.PostProcessing
import scala.language.experimental.macros
import scala.language.existentials
import scala.reflect.macros.Context
import scala.slick.jdbc.JdbcBackend
import scala.slick.driver.JdbcDriver

package object yy {
  def shallow[T](block: => T): T = macro implementations.slick[T]
  def shallowDebug[T](block: => T): T = macro implementations.slickDebug[T]
  def shallowTemplate[T](block: => T): T = macro implementations.slickTemplate[T]
  def shallowTemplateDebug[T](block: => T): T = macro implementations.slickTemplateDebug[T]
  def templateMaker[P, R](block: P => Shallow.QueryTemplate[_, R]): Shallow.QueryTemplate[P, R] = macro implementations.templateMaker[P, R]

  object implementations {
    def slick[T](c: Context)(block: c.Expr[T]): c.Expr[T] = {
      //      println(c.universe.showRaw(block))
      val yyTranformers = new {
        val universe: c.universe.type = c.universe
        val mirror = c.mirror
      } with YYTransformers
      val virtualSymbols = yyTranformers.VirtualClassCollector(block.tree)
      val virtualTypes = virtualSymbols.map(_.typeSignature)
      val virtualStatements = yyTranformers.ClassVirtualization.getStatementsFromTables

      YYTransformer[c.type, T](c)("scala.slick.yy.SlickYinYang",
        new SlickTypeTransformer[c.type](c)(virtualTypes),
        postProcessing = Some(new PostProcessing[c.type](c)(virtualStatements)),
        Map("shallow" -> false, "debug" -> 0, "featureAnalysing" -> false, "ascriptionTransforming" -> true)
      )(block)
    }
    def slickDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] = {
      //      println(c.universe.showRaw(block))
      val yyTranformers = new {
        val universe: c.universe.type = c.universe
      } with YYTransformers
      val virtualSymbols = yyTranformers.VirtualClassCollector(block.tree)
      val virtualTypes = virtualSymbols.map(_.typeSignature)
      val virtualStatements = yyTranformers.ClassVirtualization.getStatementsFromTables

      YYTransformer[c.type, T](c)("scala.slick.yy.SlickYinYang",
        new SlickTypeTransformer[c.type](c, 1)(virtualTypes),
        postProcessing = Some(new PostProcessing[c.type](c)(virtualStatements)),
        Map("shallow" -> false, "debug" -> 1, "featureAnalysing" -> false, "ascriptionTransforming" -> true)
      )(block)
    }
    def slickTemplate[T](c: Context)(block: c.Expr[T]): c.Expr[T] = {
      //      println(c.universe.showRaw(block))
      val yyTranformers = new {
        val universe: c.universe.type = c.universe
        val mirror = c.mirror
      } with YYTransformers
      val virtualSymbols = yyTranformers.VirtualClassCollector(block.tree)
      val virtualTypes = virtualSymbols.map(_.typeSignature)
      val virtualStatements = yyTranformers.ClassVirtualization.getStatementsFromTables

      YYTransformer[c.type, T](c)("scala.slick.yy.SlickYinYangTemplate",
        new SlickTypeTransformer[c.type](c)(virtualTypes),
        postProcessing = Some(new PostProcessing[c.type](c)(virtualStatements)),
        Map("shallow" -> false, "debug" -> 0, "featureAnalysing" -> false, "ascriptionTransforming" -> true, "liftTypes" -> List("slick.yy.Shallow.Query", "slick.yy.Shallow.QueryTemplate"))
      )(block)
    }
    def templateMaker[P, R](c: Context)(block: c.Expr[(P => Shallow.QueryTemplate[_, R])]): c.Expr[Shallow.QueryTemplate[P, R]] = {
      import c.universe._
//      println(c.universe.showRaw(block))
      block.tree match {
        case Function(vparams, body) =>
          val yyTranformers = new {
            val universe: c.universe.type = c.universe
            val mirror = c.mirror
          } with YYTransformers
          val virtualSymbols = yyTranformers.VirtualClassCollector(body)
          val virtualTypes = virtualSymbols.map(_.typeSignature)
          val virtualStatements = yyTranformers.ClassVirtualization.getStatementsFromTables
          //          c.universe.captureVariable(vparams.head.symbol)

          val result = YYTransformer[c.type, Shallow.QueryTemplate[P, R]](c)("scala.slick.yy.SlickYinYangTemplate",
            new SlickTypeTransformer[c.type](c)(virtualTypes),
            postProcessing = Some(new PostProcessing[c.type](c)(virtualStatements)),
            Map("shallow" -> false, "debug" -> 0, "featureAnalysing" -> false, "ascriptionTransforming" -> true, "liftTypes" -> List("slick.yy.Shallow.Query", "slick.yy.Shallow.QueryTemplate"), "noInterpretParams" -> true)
          )(c.Expr[Shallow.QueryTemplate[P, R]](body))
          val shallowQueryType = Select(Select(Select(Select(Ident(newTermName("scala")), newTermName("slick")), newTermName("yy")), newTermName("Shallow")), newTypeName("QueryTemplate"))
          val shallowQueryTemplateType = TypeTree().setOriginal(AppliedTypeTree(shallowQueryType, List(TypeTree(vparams.head.tpt.tpe), TypeTree(body.tpe.asInstanceOf[TypeRef].args(1)))))
          val newBody = TypeApply(Select(result.tree, newTermName("asInstanceOf")), List(shallowQueryTemplateType))
          c.Expr[Shallow.QueryTemplate[P, R]](c.resetAllAttrs(newBody))
        //          //          c.Expr[R](body).asInstanceOf[c.Expr[Shallow.QueryTemplate[P, R]]]
        //          //          c.Expr[Shallow.QueryTemplate[P, R]](body)
        //          val shallowQueryType = Select(Select(Select(Select(Ident("scala"), newTermName("slick")), newTermName("yy")), newTermName("Shallow")), newTypeName("QueryTemplate"))
        //          //          val shallowQueryTemplateType = AppliedTypeTree(TypeTree(typeOf[Shallow.QueryTemplate[_, _]].typeConstructor), List(TypeTree(vparams.head.tpt.tpe), TypeTree(body.tpe)))
        //          val shallowQueryTemplateType = TypeTree().setOriginal(AppliedTypeTree(shallowQueryType, List(TypeTree(vparams.head.tpt.tpe), TypeTree(body.tpe))))
        //          val newBody = TypeApply(Select(body, newTermName("asInstanceOf")), List(shallowQueryTemplateType))
        //          //          val res = c.Expr[Shallow.QueryTemplate[P, R]](c.resetAllAttrs(newBody))
        //          val res = c.Expr[Shallow.QueryTemplate[P, R]](c.resetLocalAttrs(newBody))
        //          println(res)
        //          res
        case _ => throw new SlickException("input block should had been a function")
      }
      //      val yyTranformers = new {
      //        val universe: c.universe.type = c.universe
      //        val mirror = c.mirror
      //      } with YYTransformers
      //      val virtualSymbols = yyTranformers.VirtualClassCollector(block.tree)
      //      val virtualTypes = virtualSymbols.map(_.typeSignature)
      //      val virtualStatements = yyTranformers.ClassVirtualization.getStatementsFromTables
      //
      //      YYTransformer[c.type, T](c)("scala.slick.yy.SlickYinYangTemplate",
      //        new SlickTypeTransformer[c.type](c)(virtualTypes),
      //        postProcessing = Some(new PostProcessing[c.type](c)(virtualStatements)),
      //        Map("shallow" -> false, "debug" -> 0, "featureAnalysing" -> false, "ascriptionTransforming" -> true, "liftTypes" -> List("slick.yy.Shallow.Query", "slick.yy.Shallow.QueryTemplate"))
      //      )(block)
    }
    def slickTemplateDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] = {
      //      println(c.universe.showRaw(block))
      val yyTranformers = new {
        val universe: c.universe.type = c.universe
      } with YYTransformers
      val virtualSymbols = yyTranformers.VirtualClassCollector(block.tree)
      val virtualTypes = virtualSymbols.map(_.typeSignature)
      val virtualStatements = yyTranformers.ClassVirtualization.getStatementsFromTables

      YYTransformer[c.type, T](c)("scala.slick.yy.SlickYinYangTemplate",
        new SlickTypeTransformer[c.type](c, 1)(virtualTypes),
        postProcessing = Some(new PostProcessing[c.type](c)(virtualStatements)),
        Map("shallow" -> false, "debug" -> 1, "featureAnalysing" -> false, "ascriptionTransforming" -> true, "liftTypes" -> List("slick.yy.Shallow.Query", "slick.yy.Shallow.QueryTemplate"))
      )(block)
    }

  }
}
