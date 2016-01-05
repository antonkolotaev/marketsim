package marketsim
package ops

object IfThenElse {

    def Opt[T](condition  : Option[Boolean],
               thenBranch : Option[T],
               elseBranch : Option[T])
              (implicit m : Manifest[T]) =
    {
        condition flatMap { c => if (c) thenBranch else elseBranch }
    }

    def Signal[T](condition  : reactive.Signal[Boolean],
                  thenBranch : reactive.Signal[T],
                  elseBranch : reactive.Signal[T])
                 (implicit m : Manifest[T]) =
    {
        reactive.IfThenElse(condition, thenBranch, elseBranch)
    }

    def IsSome[T](x : reactive.Signal[Option[T]])(implicit m : Manifest[T]) = reactive.Unary(x, s"$x.IsSome") { _.nonEmpty }
    def GetSome[T](x : reactive.Signal[Option[T]])(implicit m : Manifest[T]) = reactive.Unary(x, s"$x.IsSome") { _.get }

    def SignalOpt[T](condition  : reactive.Signal[Option[Boolean]],
                     thenBranch : reactive.Signal[Option[T]],
                     elseBranch : reactive.Signal[Option[T]])
                    (implicit m : Manifest[T]) =
    {
        reactive.IfThenElse(IsSome(condition),
            Signal(GetSome(condition), thenBranch, elseBranch),
            reactive.Constant(None : Option[T]))
    }

    case class Func[T](condition  : () => Boolean,
                       thenBranch : () => T,
                       elseBranch : () => T)
                      (implicit m : Manifest[T]) extends (() => T)
    {
        def apply() =
            if (condition()) thenBranch() else elseBranch()
    }

    case class FuncOpt[T](condition  : () => Option[Boolean],
                          thenBranch : () => Option[T],
                          elseBranch : () => Option[T])
                         (implicit m : Manifest[T]) extends (() => Option[T])
    {
        def apply() =
            condition() flatMap { c => if (c) thenBranch() else elseBranch() }
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
        (ctx : Context) => Func(condition(ctx), thenBranch(ctx), elseBranch(ctx))
    }

    def UnboundFuncOpt[T](condition  : Unbound[() => Option[Boolean]],
                          thenBranch : Unbound[() => Option[T]],
                          elseBranch : Unbound[() => Option[T]])
                         (implicit m : Manifest[T]) : Unbound[() => Option[T]] =
    {
        (ctx : Context) => FuncOpt(condition(ctx), thenBranch(ctx), elseBranch(ctx))
    }

}
