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

}
