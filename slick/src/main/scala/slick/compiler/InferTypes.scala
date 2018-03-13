package slick.compiler

/** Infer all missing types. */
import slick.compiler.CompilerState
class InferTypes extends Phase {
  val name = "inferTypes"

  def apply(state: CompilerState): CompilerState = state.map(_.infer(typeChildren = true))
}
