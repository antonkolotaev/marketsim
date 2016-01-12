package marketsim
package ops

import memoization.memo

object Implicits {

    import conversions.Implicits._

    trait HasMinus[T]
    {
        def minus(x : T, y : T) : T
    }

    @memo
    implicit def numericHasMinus[T](implicit m : Numeric[T]) : HasMinus[T] = new HasMinus[T] {
        def minus(x : T, y : T) = m minus (x,y)
    }

    @memo
    implicit def optionHasMinus[T](implicit ev : HasMinus[T]) : HasMinus[Option[T]] =
        new HasMinus[Option[T]] {
            def minus(x : Option[T], y : Option[T]) : Option[T] = (x,y) match {
                case (Some(a), Some(b)) => Some(ev.minus(a,b))
                case _ => None
            }
        }

    implicit class RichOption[T](x : Option[T])
    {
        def - (y : Option[T])(implicit m : HasMinus[Option[T]]) = m minus (x,y)
    }

    @memo
    def MinusFunc[T](x : () => T, y : () => T)(implicit m : HasMinus[T]) : () => T =
    {
        () => m minus (x(), y())
    }

    @memo
    def MinusSignal[T](x : reactive.Signal[T],
                       y : reactive.Signal[T])(implicit m : HasMinus[T]) : reactive.Signal[T] =
    {
        reactive.Binary(x,y,"-") { case (a,b) => m minus (a,b) }
    }

    implicit class RichFunction[T](x : () => T)
    {
        def - [R](y : R)
                 (implicit m : HasMinus[T],
                           c : ConversionFuncSig[R, () => T])
            = MinusFunc(x,c convert y)

        /*def -: [R](y : R)
                  (implicit m : HasMinus[R],
                            c : ConversionUnbound[() => T, R])
        = MinusFunc[R](c convert x,y) */
    }

    implicit class RichSignal[T](x : reactive.Signal[T])
    {
        def - [R](y : R)
                 (implicit m : HasMinus[T],
                  c : ConversionFuncSig[R, reactive.Signal[T]])
        = MinusSignal(x,c convert y)

        /*def -: [R](y : R)
                  (implicit m : HasMinus[R],
                            c : ConversionUnbound[() => T, R])
        = MinusFunc[R](c convert x,y) */
    }


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
