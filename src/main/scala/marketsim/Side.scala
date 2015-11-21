package marketsim

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
