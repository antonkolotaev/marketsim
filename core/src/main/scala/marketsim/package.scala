import memoization.memo

package object marketsim {

    trait Context

    type Unbound[T] = Context => T

    @memo def unbound[T : Manifest](x : T) : Context => T = (ctx : Context) => x

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

    class CachedFunction[T](f : () => T) extends (() => T)
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

    object CachedFunction
    {
        @memo
        def apply[T](f : () => T) : (() => T) = new CachedFunction(f)
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


    def none[T] : Option[T] = None


    def some[T](x : T) : Option[T] = Some(x)

    object Const {
        @memo
        def apply[T : Manifest](x : T) : (() => T) = new (() => T) {
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
