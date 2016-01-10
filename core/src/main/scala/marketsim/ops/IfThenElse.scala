package marketsim
package ops

import memoization.memo

object IfThenElse {

    def Opt[T](condition  : Option[Boolean],
               thenBranch : Option[T],
               elseBranch : Option[T])
              (implicit m : Manifest[T]) =
    {
        condition flatMap { c => if (c) thenBranch else elseBranch }
    }

    def Signal[T](condition  : reactive.Signal[Boolean],
                  thenBranch : reactive.Signal[T])
                 (implicit m : Manifest[T]) = new
        {
            def Else(elseBranch : reactive.Signal[T]) =
                reactive.IfThenElse(condition, thenBranch, elseBranch)
        }

    def SignalOpt[T](condition  : reactive.Signal[Option[Boolean]],
                     thenBranch : reactive.Signal[Option[T]])
                    (implicit m : Manifest[T]) = new
    {
        def Else(elseBranch : reactive.Signal[Option[T]]) =
            reactive.IfThenElse(condition.isSome,
                Signal(condition.getSome, thenBranch).Else(elseBranch),
                reactive.Constant(None : Option[T]))
    }

    def Func[T](condition  : () => Boolean,
                thenBranch : () => T)
                (implicit m : Manifest[T]) = new
    {
        @memo
        def Else(elseBranch : () => T) : () => T =
            () => if (condition()) thenBranch() else elseBranch()
    }

    def FuncOpt[T](condition  : () => Option[Boolean],
                   thenBranch : () => Option[T])
                  (implicit m : Manifest[T]) = new
    {
        @memo
        def Else(elseBranch : () => Option[T]) : () => Option[T] =
            () => condition() flatMap { c => if (c) thenBranch() else elseBranch() }
    }

    def UnboundOpt[T](condition  : Unbound[Option[Boolean]],
                      thenBranch : Unbound[Option[T]],
                      elseBranch : Unbound[Option[T]])
                     (implicit m : Manifest[T]) =
    {
        (ctx : Context) => Opt(condition(ctx), thenBranch(ctx), elseBranch(ctx))
    }

    def UnboundFunc[T](condition  : Unbound[() => Boolean],
                       thenBranch : Unbound[() => T],
                       elseBranch : Unbound[() => T])
                      (implicit m : Manifest[T]) : Unbound[() => T] =
    {
        (ctx : Context) => Func(condition(ctx), thenBranch(ctx)).Else(elseBranch(ctx))
    }

    def UnboundFuncOpt[T](condition  : Unbound[() => Option[Boolean]],
                          thenBranch : Unbound[() => Option[T]],
                          elseBranch : Unbound[() => Option[T]])
                         (implicit m : Manifest[T]) : Unbound[() => Option[T]] =
    {
        (ctx : Context) => FuncOpt(condition(ctx), thenBranch(ctx)).Else(elseBranch(ctx))
    }

}
