package marketsim.orderbook

package object linear {

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

    trait TickMapper[Currency]
    {
        def toTicks(x : Currency, side : Side) : Ticks
        def toCurrency(x : Ticks) : Currency
    }

    case class USD(centicents : Int)
    {
        override def toString = "$" + (centicents / 10000.0)

        def * (x : Int) = USD(centicents * x)
        def + (x : USD) = USD(centicents + x.centicents)
        def - (x : USD) = USD(centicents - x.centicents)
    }

    def cents(x : Int) = USD(x * 100)

    implicit val zeroUSD = cents(0)

    class LinearMapper(tickSize : USD) extends TickMapper[USD]
    {
        def toTicks(x : USD, side : Side) = side match {
            case Buy => Ticks(math.floor(x.centicents / tickSize.centicents).toInt)
            case Sell => Ticks(math.ceil(x.centicents / tickSize.centicents).toInt)
        }

        def toCurrency(x : Ticks) = USD(x.value * tickSize.centicents)
    }
    
    case class Traded(price : SignedTicks, volume : Quantity)
    {
        def side = price.side
    }

    case class Cancelled(side : Side, amount : Quantity)
    case class Completed()


    /**
     *  Interface for order event listeners
     */
    trait OrderListener {
        def handle(traded : Traded) {}
        def handle(cancelled : Cancelled) {}
        def handle(completed : Completed) {}
    }

    class OrderListenerProxy(target : OrderListener) extends OrderListener
    {
        override def handle(traded : Traded) = target handle traded
        override def handle(cancelled: Cancelled) = target handle cancelled
        override def handle(completed : Completed) = target handle completed
    }

    case class TradeDone(price : SignedTicks, volume : Quantity)

    trait AbstractOrderQueue[Currency]
    {
        val priceLevels : marketsim.reactive.Signal[List[(Ticks, Currency, Quantity)]]

        val tradeDone = new marketsim.reactive.Event[TradeDone]
    }

    case class BestPrice[Currency](queue : AbstractOrderQueue[Currency])
        extends marketsim.reactive.UnaryBase(queue.priceLevels, Option.empty[Ticks], s"BestPrice($queue)")
    {
        def F(a : List[(Ticks, Currency, Quantity)]) = a.headOption map { _._1 }
    }

    case class BestPriceCurrency[Currency](queue : AbstractOrderQueue[Currency])
        extends marketsim.reactive.UnaryBase(queue.priceLevels, Option.empty[Currency], s"BestPriceCurrency($queue)")
    {
        def F(a : List[(Ticks, Currency, Quantity)]) = a.headOption map { _._2 }
    }

    case class BestPriceVolume[Currency](queue : AbstractOrderQueue[Currency])
        extends marketsim.reactive.UnaryBase(queue.priceLevels, Option.empty[Quantity], s"BestPriceVolume($queue)")
    {
        def F(a : List[(Ticks, Currency, Quantity)]) = a.headOption map { _._3 }
    }

    trait AbstractOrderBook[Currency]
    {
        val Asks : AbstractOrderQueue[Currency]
        val Bids : AbstractOrderQueue[Currency]
        val tickMapper : TickMapper[Currency]
        def process(order : LimitOrder)
        def process(order : MarketOrder)
        def cancel(token : Canceller, amountToCancel : Quantity)
        def fetchPriceLevelsTillVolume(limitVolume : Quantity)
    }

    trait AbstractOrder
    {
        val side : Side
        val volume : Quantity
    }

    case class MarketOrder(side : Side, volume : Quantity, sender : OrderListener) extends AbstractOrder

    case class LimitOrder(side              : Side,
                          price             : Ticks,
                          volume            : Quantity,
                          sender            : OrderListener,
                          cancellationKey   : Option[Canceller] = None) extends AbstractOrder

    def cancellableLimitOrder(side              : Side,
                              price             : Ticks,
                              volume            : Quantity,
                              sender            : OrderListener) =
        LimitOrder(side, price, volume, sender, Some(new Canceller))

    case class LimitOrderInfo(side : Side, price : SignedTicks, unmatchedVolume : Quantity, sender : OrderListener)

}
