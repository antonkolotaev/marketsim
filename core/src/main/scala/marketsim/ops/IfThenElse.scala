package marketsim
package ops

import memoization.memo

object IfThenElse {

    case class Opt[T](condition  : Option[Boolean],
                     thenBranch : Option[T])
                    (implicit m : Manifest[T])
    {
        def Else(elseBranch : Option[T]) =
            condition flatMap { c => if (c) thenBranch else elseBranch }
    }

    case class Signal[T](condition  : reactive.Signal[Boolean],
                         thenBranch : reactive.Signal[T])
                        (implicit m : Manifest[T])
    {
        def Else(elseBranch : reactive.Signal[T]) =
            reactive.IfThenElse(condition, thenBranch, elseBranch)
    }

    case class SignalOpt[T](condition  : reactive.Signal[Option[Boolean]],
                            thenBranch : reactive.Signal[Option[T]])
                           (implicit m : Manifest[T])
    {
        def Else(elseBranch : reactive.Signal[Option[T]]) =
            reactive.IfThenElse(condition.isSome,
                Signal(condition.getSome, thenBranch).Else(elseBranch),
                reactive.Constant(None : Option[T]))
    }

    @memo
    def Function[T](condition : () => Boolean,
                    thenBranch : () => T,
                    elseBranch : () => T)
                   (implicit m : Manifest[T]) : () => T =
    {
        () => if (condition()) thenBranch() else elseBranch()
    }


    case class Func[T](condition  : () => Boolean,
                       thenBranch : () => T)
                      (implicit m : Manifest[T])
    {
        @memo
        def Else(elseBranch : () => T) : () => T =
            () => if (condition()) thenBranch() else elseBranch()
    }

    case class FuncOpt[T](condition  : () => Option[Boolean],
                          thenBranch : () => Option[T])
                         (implicit m : Manifest[T])
    {
        @memo
        def Else(elseBranch : () => Option[T]) : () => Option[T] =
            () => condition() flatMap { c => if (c) thenBranch() else elseBranch() }
    }

    @memo
    def UnboundOpt[T](condition  : Unbound[Option[Boolean]],
                      thenBranch : Unbound[Option[T]],
                      elseBranch : Unbound[Option[T]])
                      (implicit m : Manifest[T]) : Unbound[Option[T]] =
    {
        (ctx : Context) => Opt(condition(ctx), thenBranch(ctx)) Else elseBranch(ctx)
    }

    case class UnboundFunc[T](condition  : Unbound[() => Boolean],
                             thenBranch : Unbound[() => T])
                            (implicit m : Manifest[T])
    {
        @memo
        def Else(elseBranch : Unbound[() => T]) : Unbound[() => T] =
            (ctx : Context) => Func(condition(ctx), thenBranch(ctx)) Else elseBranch(ctx)
    }

    case class UnboundFuncOpt[T](condition  : Unbound[() => Option[Boolean]],
                                 thenBranch : Unbound[() => Option[T]])
                                (implicit m : Manifest[T])
    {
        @memo
        def Else(elseBranch : Unbound[() => Option[T]]) : Unbound[() => Option[T]] =
            (ctx : Context) => FuncOpt(condition(ctx), thenBranch(ctx)) Else elseBranch(ctx)
    }

    case class UnboundSignal[T](condition  : Unbound[reactive.Signal[Boolean]],
                                thenBranch : Unbound[reactive.Signal[T]])
                               (implicit m : Manifest[T])
    {
        @memo
        def Else(elseBranch : Unbound[reactive.Signal[T]]) : Unbound[reactive.Signal[T]] =
            (ctx : Context) => Signal(condition(ctx), thenBranch(ctx)) Else elseBranch(ctx)
    }

    case class UnboundSignalOpt[T](condition  : Unbound[reactive.Signal[Option[Boolean]]],
                                   thenBranch : Unbound[reactive.Signal[Option[T]]])
                                  (implicit m : Manifest[T])
    {
        @memo
        def Else(elseBranch : Unbound[reactive.Signal[Option[T]]]) : Unbound[reactive.Signal[Option[T]]] =
            (ctx : Context) => SignalOpt(condition(ctx), thenBranch(ctx)) Else elseBranch(ctx)
    }

}
