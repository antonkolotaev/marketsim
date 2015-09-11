package orderbook

package object linear {

    case class Ticks(value : Int)
    {
        def signed(side : Side) = side makeSigned this
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

    trait Order
    {
        val side    : Side
        val volume  : Quantity
    }

    val TerminalOrderPrice = SignedTicks(Int.MaxValue)
    val MarketOrderPrice = TerminalOrderPrice moreAggressiveBy 1

    class Canceller
    {
        private var entry_level = Option.empty[(Entry, SamePriceOrders)]

        private[linear] def set(e : Entry, level : SamePriceOrders) = {
            entry_level = Some((e, level))
        }

        def side = entry_level map { _._2.price.side }

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

    class OrderListenerProxy(target : OrderListener) extends OrderListener
    {
        override def traded(price : Ticks, amount : Quantity) = target traded (price, amount)
        override def cancelled(amount : Quantity) = target cancelled amount
        override def completed() = target completed ()
    }

    trait AbstractOrderQueue
    {
        val bestPrice : reactive.Value[Option[Ticks]]
        val bestPriceVolume : reactive.Value[Option[Quantity]]
        val lastTrade : reactive.Value[Option[(Ticks, Quantity)]]
        val lastTrades : reactive.Value[List[(Ticks, Quantity)]]
    }

    trait AbstractOrderBook
    {
        val Asks : AbstractOrderQueue
        val Bids : AbstractOrderQueue
        def process(order : LimitOrder)
        def process(order : MarketOrder)
        def cancel(token : Canceller, amountToCancel : Quantity)
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
