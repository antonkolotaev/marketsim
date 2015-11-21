
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

    trait Side
    {
        /**
         * aggressive(priceA) > aggressive(priceB) => signed(priceA) < signed(priceB)
         * Sell side: greater price is less aggressive => keep it
         * Buy side: greater price is more aggressive => negate it
         *
         * To get more aggressive signed price one need to subtract positive delta from it
         */
        def makeSigned(price : Ticks) : SignedTicks

        def opposite : Side
    }

    object Side {
        def choices = Sell :: Buy :: Nil
    }

    case object Sell extends Side
    {
        def makeSigned(price : Ticks) = SignedTicks(price.value)
        def opposite = Buy
    }

    case object Buy extends Side
    {
        def makeSigned(price : Ticks) = SignedTicks(-price.value)
        def opposite = Sell
    }


}
