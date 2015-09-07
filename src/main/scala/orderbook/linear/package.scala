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

        def opposite : Side
    }
    
    object Side {
        def of(price : Ticks) = if (price < 0) Buy else Sell
        def choices = Sell :: Buy :: Nil
    }

    case object Sell extends Side
    {
        def makeSigned(price : Ticks) = price
        def opposite = Buy
    }

    case object Buy extends Side
    {
        def makeSigned(price : Ticks) = -price
        def opposite = Sell
    }

    trait Order
    {
        val side    : Side
        val volume  : Quantity
    }

    val TerminalOrderPrice = Int.MaxValue
    val MarketOrderPrice = TerminalOrderPrice - 1

    class Canceller
    {
        private var entry_level = Option.empty[(Entry, SamePriceOrders)]

        private[linear] def set(e : Entry, level : SamePriceOrders) = {
            entry_level = Some((e, level))
        }

        private[linear] def apply(amountToCancel : Quantity) = {
            entry_level match {
                case Some((entry, level)) => level cancel (entry, amountToCancel)
                case None =>
                    //throw new Exception("CancellationKeyImpl is not initialized")
            }
        }
    }

    /**
     *  Interface for order event listeners
     */
    trait OrderListener {
        def traded(price : Ticks, amount : Quantity) {}
        def cancelled(amount : Quantity) {}
        def completed() {}
    }

    case class MarketOrder(side : Side, volume : Quantity, sender : OrderListener) extends Order

    case class LimitOrder(side              : Side,
                          price             : Ticks,
                          volume            : Quantity,
                          sender            : OrderListener,
                          cancellationKey   : Option[Canceller] = None) extends Order

    def cancellableLimitOrder(side              : Side,
                              price             : Ticks,
                              volume            : Quantity,
                              sender            : OrderListener) =
        LimitOrder(side, price, volume, sender, Some(new Canceller))

    case class LimitOrderInfo(side : Side, price : SignedTicks, unmatchedVolume : Quantity, sender : OrderListener)

}
