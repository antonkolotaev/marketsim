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
        (ctx : Context) => Opt(condition(ctx), thenBranch(ctx), elseBranch(ctx))
    }

    @memo
    def UnboundFunc[T](condition  : Unbound[() => Boolean],
                       thenBranch : Unbound[() => T],
                       elseBranch : Unbound[() => T])
                      (implicit m : Manifest[T]) : Unbound[() => T] =
    {
        (ctx : Context) => condition(ctx) Then thenBranch(ctx) Else elseBranch(ctx)
    }

    @memo
    def UnboundFuncOpt[T](condition  : Unbound[() => Option[Boolean]],
                          thenBranch : Unbound[() => Option[T]],
                          elseBranch : Unbound[() => Option[T]])
                         (implicit m : Manifest[T]) : Unbound[() => Option[T]] =
    {
        (ctx : Context) => condition(ctx) Then thenBranch(ctx) Else elseBranch(ctx)
    }

    @memo
    def  UnboundSignal[T](condition  : Unbound[reactive.Signal[Boolean]],
                          thenBranch : Unbound[reactive.Signal[T]],
                          elseBranch : Unbound[reactive.Signal[T]])
                         (implicit m : Manifest[T]) : Unbound[reactive.Signal[T]] =
    {
        (ctx : Context) => condition(ctx) Then thenBranch(ctx) Else elseBranch(ctx)
    }

    @memo
    def UnboundSignalOpt[T](condition  : Unbound[reactive.Signal[Option[Boolean]]],
                            thenBranch : Unbound[reactive.Signal[Option[T]]],
                            elseBranch : Unbound[reactive.Signal[Option[T]]])
                            (implicit m : Manifest[T]) : Unbound[reactive.Signal[Option[T]]] =
    {
        (ctx : Context) => condition(ctx) Then thenBranch(ctx) Else elseBranch(ctx)
    }

}
