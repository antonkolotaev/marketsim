package com.softwaremill.macmemo

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros

@compileTimeOnly("enable macro paradise to expand macro annotations")
class memo
    extends StaticAnnotation {

  def macroTransform(annottees: Any*): Any = macro memoizeMacro.impl
}
