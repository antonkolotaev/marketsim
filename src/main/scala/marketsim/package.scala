
package object marketsim {

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

    case class USD(centicents: Int) {
        override def toString = "$" + (centicents / 10000.0)

        def *(x: Int) = USD(centicents * x)

        def +(x: USD) = USD(centicents + x.centicents)

        def -(x: USD) = USD(centicents - x.centicents)
    }

    def cents(x: Int) = USD(x * 100)

    implicit val zeroUSD = cents(0)

}
