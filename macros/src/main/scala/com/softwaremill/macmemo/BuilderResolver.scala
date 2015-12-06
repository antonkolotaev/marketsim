package com.softwaremill.macmemo

import scala.language.experimental.macros

object BuilderResolver {

  def resolve2(methodFullName: String): MemoCacheBuilder2 = macro builderResolverMacro_impl2

  def builderResolverMacro_impl2(c: scala.reflect.macros.whitebox.Context)(methodFullName: c.Expr[String]): c.Expr[MemoCacheBuilder2] = {
    import c.universe._

    def bringDefaultBuilder: Tree = {
      val Literal(Constant(mfn: String)) = methodFullName.tree
      val msg = s"Cannot find custom memo builder for '$mfn' - default builder will be used"
      c.info(c.enclosingPosition, msg, false)
      reify {
        GlobalCache.Builder
      }.tree
    }

    val builderTree = c.inferImplicitValue(typeOf[MemoCacheBuilder2]) match {
      case EmptyTree => bringDefaultBuilder
      case foundBuilderTree => foundBuilderTree
    }

    c.Expr[MemoCacheBuilder2](Block(List(), builderTree))
  }
}
