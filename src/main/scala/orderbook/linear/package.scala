package orderbook

package object linear {

    type Ticks = Int
    type SignedTicks = Int
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
    }
    
    object Side {
        def of(price : Ticks) = if (price < 0) Buy else Sell
        def choices = Sell :: Buy :: Nil
    }

    case object Sell extends Side
    {
        def makeSigned(price : Ticks) = price
    }

    case object Buy extends Side
    {
        def makeSigned(price : Ticks) = -price
    }

    trait Order
    {
        val side    : Side
        val volume  : Quantity
    }

    val TerminalOrderPrice = Int.MaxValue
    val MarketOrderPrice = TerminalOrderPrice - 1

    type CancellationToken = Quantity => Quantity

    /**
     *  Interface for order event listeners
     */
    trait OrderListener {
        def traded(price : Ticks, amount : Quantity) {}
        def cancelled(amount : Quantity) {}
        def completed() {}
    }

    case class MarketOrder(side : Side, volume : Quantity) extends Order

    case class LimitOrder(side : Side, price : SignedTicks, volume : Quantity) extends Order

    case class LimitOrderInfo(side : Side, price : SignedTicks, unmatchedVolume : Quantity, sender : OrderListener)

}
