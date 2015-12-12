package memoization

/**
 * Based on MacMemo https://github.com/kciesielski/macmemo
 */


import scala.reflect.macros._

object memoizeMacro {
  private val debug = new Debug()

  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*) = {
    import c.universe._

    case class MemoIdentifier(methodName: TermName, generatedMemoValName: TermName)

    def reportInvalidAnnotationTarget() {
      c.error(c.enclosingPosition, "This annotation can only be used on methods")
    }

    def prepareInjectedBody(cachedMethodId: MemoIdentifier, valDefs: List[List[ValDef]], bodyTree: Tree, returnTypeTree: Tree): c.type#Tree = {
      val names = valDefs.flatten.map(_.name)
      q"""
      def callRealBody() = { $bodyTree }
      ${cachedMethodId.generatedMemoValName}.get($names, {
        List(
          callRealBody()
        )
      }).head.asInstanceOf[$returnTypeTree]
      """
    }

    def createMemoVal(cachedMethodId: MemoIdentifier, returnTypeTree: Tree): c.type#Tree = {

      val enclosure = c.enclosingClass

      def buildCacheBucketId: Tree = {
        val enclosingClassSymbol = enclosure.symbol
        val enclosureFullName = enclosingClassSymbol.fullName + (if (enclosingClassSymbol.isModule) "$." else ".")
        Literal(Constant(
          enclosureFullName + cachedMethodId.methodName.toString))
      }

      q"""lazy val ${cachedMethodId.generatedMemoValName}: memoization.Cache[List[Any]] =
         memoization.BuilderResolver.resolve($buildCacheBucketId).build($buildCacheBucketId)"""

    }

    def injectCacheUsage(cachedMethodId: MemoIdentifier, function: DefDef) = {
      val DefDef(mods, name, tparams, valDefs, returnTypeTree, bodyTree) = function
      val injectedBody = prepareInjectedBody(cachedMethodId, valDefs, bodyTree, returnTypeTree)
      DefDef(mods, name, tparams, valDefs, returnTypeTree, injectedBody)
    }

    val inputs = annottees.map(_.tree).toList
    val (_, expandees) = inputs match {
      case (functionDefinition: DefDef) :: rest =>
        debug(s"Found annotated function [${functionDefinition.name}]")
        val DefDef(_, name: TermName, p, r, returnTypeTree, q) = functionDefinition
        val NAME = name + q.toString()
        val cachedMethodIdentifier = MemoIdentifier.apply(NAME, TermName(c.freshName(s"memo_${NAME}_")))
        val memoVal = createMemoVal(cachedMethodIdentifier, returnTypeTree)
        val newFunctionDef = injectCacheUsage(cachedMethodIdentifier, functionDefinition)
        (functionDefinition, memoVal +: (newFunctionDef :: rest))
      case _ => reportInvalidAnnotationTarget(); (EmptyTree, inputs)
    }

    debug(s"final method = ${show(expandees)}")

    c.Expr[Any](Block(expandees, Literal(Constant(()))))
  }


}

