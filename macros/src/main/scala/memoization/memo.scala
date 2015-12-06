package memoization

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros

/**
 * Based on MacMemo https://github.com/kciesielski/macmemo
 */

@compileTimeOnly("enable macro paradise to expand macro annotations")
class memo
    extends StaticAnnotation {

  def macroTransform(annottees: Any*): Any = macro memoizeMacro.impl
}
