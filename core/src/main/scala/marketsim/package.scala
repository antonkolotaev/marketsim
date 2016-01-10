import memoization.memo

package object marketsim {

    trait Context

    type Unbound[T] = Context => T

    @memo def unbound[T](x : T) : Context => T = (ctx : Context) => x

    case class Time(x: Long) {
        def <(other: Time) = x < other.x

        def <=(other: Time) = x <= other.x

        def +(dt: Duration) = Time(x + dt.x)

        override def toString = s"${x}s"
    }

    case class Duration(x: Long) {
        def +(other: Duration) = Duration(x + other.x)

        override def toString = s"${x}s"
    }

    case class Ticks(value : Int)
    {
        def signed(side : Side) = side makeSigned this

        override def toString = s"${value}pts"
    }

    case class SignedTicks(value : Int)
    {
        def ticks = Ticks(value.abs)
        def isMoreAggressiveThan (other : SignedTicks) = value < other.value
        def lessAggressiveBy (delta : Int) = SignedTicks(value + delta)
        def moreAggressiveBy (delta : Int) = SignedTicks(value - delta)
        def side = if (value < 0) Buy else Sell
        def opposite = SignedTicks(-value)

        override def toString = (if (value >= 0) "+" else "") + value
    }

    type Quantity = Int

    case class Currency(centicents: Int) {
        override def toString = "$" + (centicents / 10000.0)

        def *(x: Int) = Currency(centicents * x)

        def +(x: Currency) = Currency(centicents + x.centicents)

        def -(x: Currency) = Currency(centicents - x.centicents)

        def / (x : Currency) = 1.0 * centicents / x.centicents
    }

    def cents(x: Int) = Currency(x * 100)

    implicit val zeroUSD = cents(0)

    case class CachedFunction[T](f : () => T) extends (() => T)
    {
        private var lastT = Scheduler.eventSourceId

        private var cachedValue = f()

        def apply() = {
            val currentT = Scheduler.eventSourceId
            if (currentT != lastT) {
                cachedValue = f()
                lastT = currentT
            }
            cachedValue
        }
    }

    implicit class Rich[T](x : T)
    {
        def as[R](implicit c : ConversionUnbound[T, R]) = c convert x

        def asTestRefEq[R <: AnyRef](implicit c : ConversionUnbound[T, R]) = {
            val converted = as[R]
            val converted2 = as[R]
            assert(converted eq converted2)
            converted
        }
    }

    implicit class RichSignalOption[T](x : reactive.Signal[Option[T]])
    {
        @memo
        def isSome(implicit m : Manifest[T]) : reactive.Signal[Boolean] = reactive.Unary(x, s"$x.isSome") { _.nonEmpty }

        @memo
        def getSome(implicit m : Manifest[T]) : reactive.Signal[T] = reactive.Unary(x, s"$x.getSome") { _.get }
    }

    implicit class RichOptionBoolean(x : Option[Boolean])
    {
        def Then[T : Manifest](thenBranch : Option[T]) = new
            {
                def Else(elseBranch : Option[T]) =
                    x flatMap { c => if (c) thenBranch else elseBranch }
            }
    }

    implicit class RichBooleanSignal(x : reactive.Signal[Boolean])
    {
        def Then[T : Manifest](thenBranch : reactive.Signal[T]) = ops.IfThenElse.Signal(x, thenBranch)
    }

    implicit class RichOptionBooleanSignal(x : reactive.Signal[Option[Boolean]])
    {
        def Then[T : Manifest](thenBranch : reactive.Signal[Option[T]]) = ops.IfThenElse.SignalOpt(x, thenBranch)
    }

    implicit class RichBooleanFunc(x : () => Boolean)
    {
        def Then[T : Manifest](thenBranch : () => T) =
            new { def Else(elseBranch : () => T) = ops.IfThenElse.Function(x, thenBranch, elseBranch) }
    }

    implicit class RichOptionBooleanFunc(x : () => Option[Boolean])
    {
        def Then[T : Manifest](thenBranch : () => Option[T]) = ops.IfThenElse.FuncOpt(x, thenBranch)
    }

    implicit class RichUnboundOptionBoolean(x : Unbound[Option[Boolean]])
    {
        def Then[T : Manifest](thenBranch : Unbound[Option[T]]) =
            new { def Else(elseBranch : Unbound[Option[T]]) = ops.IfThenElse.UnboundOpt(x, thenBranch, elseBranch) }
    }

    implicit class RichUnboundBooleanSignal(x : Unbound[reactive.Signal[Boolean]])
    {
        def Then[T : Manifest](thenBranch : Unbound[reactive.Signal[T]]) = ops.IfThenElse.UnboundSignal(x, thenBranch)
    }

    implicit class RichUnboundOptionBooleanSignal(x : Unbound[reactive.Signal[Option[Boolean]]])
    {
        def Then[T : Manifest](thenBranch : Unbound[reactive.Signal[Option[T]]]) = ops.IfThenElse.UnboundSignalOpt(x, thenBranch)
    }

    implicit class RichUnboundBooleanFunc(x : Unbound[() => Boolean])
    {
        def Then[T : Manifest](thenBranch : Unbound[() => T]) = ops.IfThenElse.UnboundFunc(x, thenBranch)
    }

    implicit class RichUnboundOptionBooleanFunc(x : Unbound[() => Option[Boolean]])
    {
        def Then[T : Manifest](thenBranch : Unbound[() => Option[T]]) = ops.IfThenElse.UnboundFuncOpt(x, thenBranch)
    }

    def some[T](x : T) : Option[T] = Some(x)

    object Const {
        @memo
        def apply[T](x : T) : (() => T) = new (() => T) {
            def apply() = x
        }
    }

    object Compose {
        @memo
        def apply[T,R](f : () => T, g : T => R) : (() => R) = new (() => R) {
            def apply() = g(f())
        }
    }

    object Compose1 {
        @memo
        def apply[S,T,R](f : S => T, g : T => R) : (S => R) = new (S => R) {
            def apply(x : S) = g(f(x))
        }
    }
}
