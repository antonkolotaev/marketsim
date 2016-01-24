package marketsim
package ops

import memoization.memo

object Implicits
    extends Minus.Implicits
    with Plus.Implicits
    with Mul.Implicits
    with Div.Implicits
    with And.Implicits
{

    import conversions.Implicits._

    @memo
    private def isSomeImplF[T](x : () => Option[T])
                              (implicit m : Manifest[T]) : () => Boolean =
        () => x().nonEmpty

    @memo
    private def isSomeImplS[T](x : reactive.Signal[Option[T]])
                              (implicit m : Manifest[T]) : reactive.Signal[Boolean] =
        reactive.Unary(x, s"isSome") { _.nonEmpty }

    @memo
    private def getSomeImplF[T](x : () => Option[T])
                               (implicit m : Manifest[T]) : () => T =
        () => x().get

    @memo
    def getSomeImplS[T](x : reactive.Signal[Option[T]])
                       (implicit m : Manifest[T]) : reactive.Signal[T] =
        reactive.Unary(x, s"getSome") { _.get }

    implicit class RichFunctionOption[T](x : () => Option[T])
    {
        def isSome(implicit m : Manifest[T]) = isSomeImplF(x)(m)
        def getSome(implicit m : Manifest[T]) = getSomeImplF(x)(m)
    }

    implicit class RichSignalOption[T](x : reactive.Signal[Option[T]])
    {
        def isSome(implicit m : Manifest[T]) = isSomeImplS(x)(m)
        def getSome(implicit m : Manifest[T]) = getSomeImplS(x)(m)
    }

    implicit class RichOptionBoolean(x : Option[Boolean])
    {
        def Then[T : Manifest](thenBranch : Option[T]) =
            new {
                def Else[R : Manifest](elseBranch : R)
                           (implicit c : ConversionOpt[R, Option[T]]) = {
                    ops.IfThenElse.Opt(x, thenBranch, c convert elseBranch)
                }
            }
    }

    implicit class RichBooleanSignal(x : reactive.Signal[Boolean])
    {
        def Then[T : Manifest](thenBranch : reactive.Signal[T]) =
            new {
                def Else[R](elseBranch : R)
                           (implicit c : ConversionFuncSig[R, reactive.Signal[T]]) = {
                    reactive.IfThenElse(x, thenBranch, c convert elseBranch)
                }
            }
    }

    implicit class RichOptionBooleanSignal(x : reactive.Signal[Option[Boolean]])
    {
        def Then[T : Manifest](thenBranch : reactive.Signal[Option[T]]) =
            new {
                def Else[R](elseBranch : R)
                           (implicit c : ConversionFuncSig[R, reactive.Signal[Option[T]]]) =
                    (x.isSome
                        Then (x.getSome
                                Then thenBranch
                                Else (c convert elseBranch))
                        Else reactive.Constant[Option[T]](None))
            }
    }

    implicit class RichBooleanFunc(x : () => Boolean)
    {
        def Then[T : Manifest](thenBranch : () => T) =
            new {
                def Else[R](elseBranch : R)
                           (implicit c : ConversionFuncSig[R, () => T]) =
                    ops.IfThenElse.Function(x, thenBranch, c convert elseBranch)
            }
    }

    implicit class RichOptionBooleanFunc(x : () => Option[Boolean])
    {
        def Then[T : Manifest](thenBranch : () => Option[T]) =
            new {
                def Else[R](elseBranch : R)
                           (implicit c : ConversionFuncSig[R, () => Option[T]]) =
                    (x.isSome
                        Then (x.getSome
                                    Then thenBranch
                                    Else (c convert elseBranch))
                        Else Const(none[T]))
            }
    }

    implicit class RichUnboundOptionBoolean(x : Unbound[Option[Boolean]])
    {
        def Then[T : Manifest](thenBranch : Unbound[Option[T]]) =
            new {
                def Else[R](elseBranch : R)
                           (implicit c : ConversionUnbound[R, Unbound[Option[T]]]) =
                {
                    ops.IfThenElse.UnboundOpt(x, thenBranch, c convert elseBranch)
                }
            }
    }

    implicit class RichUnboundBooleanSignal(x : Unbound[reactive.Signal[Boolean]])
    {
        def Then[T : Manifest](thenBranch : Unbound[reactive.Signal[T]]) =
            new {
                def Else[R](elseBranch : R)
                           (implicit c : ConversionUnbound[R, Unbound[reactive.Signal[T]]]) =
                {
                    ops.IfThenElse.UnboundSignal(x, thenBranch, c convert elseBranch)
                }
            }
    }

    implicit class RichUnboundOptionBooleanSignal(x : Unbound[reactive.Signal[Option[Boolean]]])
    {
        def Then[T : Manifest](thenBranch : Unbound[reactive.Signal[Option[T]]]) =
            new {
                def Else[R](elseBranch: R)
                           (implicit c : ConversionUnbound[R, Unbound[reactive.Signal[Option[T]]]]) =
                {
                    ops.IfThenElse.UnboundSignalOpt(x, thenBranch, c convert elseBranch)
                }
            }
    }

    implicit class RichUnboundBooleanFunc(x : Unbound[() => Boolean])
    {
        def Then[T : Manifest](thenBranch : Unbound[() => T]) =
            new {
                def Else[R](elseBranch : R)
                           (implicit c : ConversionUnbound[R, Unbound[() => T]]) =
                {
                    ops.IfThenElse.UnboundFunc(x, thenBranch, c convert elseBranch)
                }
            }
    }

    implicit class RichUnboundOptionBooleanFunc(x : Unbound[() => Option[Boolean]])
    {
        def Then[T : Manifest](thenBranch : Unbound[() => Option[T]]) =
            new {
                def Else[R](elseBranch : R)
                           (implicit c : ConversionUnbound[R, Unbound[() => Option[T]]]) =
                {
                    ops.IfThenElse.UnboundFuncOpt(x, thenBranch, c convert elseBranch)
                }
            }
    }

}
