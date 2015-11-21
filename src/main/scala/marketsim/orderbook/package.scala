package marketsim

package object orderbook {

    trait TickMapper[Currency] {
        def toTicks(x: Currency, side: Side): Ticks

        def toCurrency(x: Ticks): Currency
    }

    class LinearMapper(tickSize: USD) extends TickMapper[USD] {
        def toTicks(x: USD, side: Side) = side match {
            case Buy => Ticks(math.floor(x.centicents / tickSize.centicents).toInt)
            case Sell => Ticks(math.ceil(x.centicents / tickSize.centicents).toInt)
        }

        def toCurrency(x: Ticks) = USD(x.value * tickSize.centicents)
    }

    trait AbstractCanceller

    case class Traded(price: SignedTicks, volume: Quantity) {
        def side = price.side
    }

    case class Cancelled(side: Side, amount: Quantity)

    case class Completed()


    /**
     * Interface for order event listeners
     */
    trait OrderListener {
        def handle(traded: Traded) {}

        def handle(cancelled: Cancelled) {}

        def handle(completed: Completed) {}
    }

    class OrderListenerProxy(target: OrderListener) extends OrderListener {
        override def handle(traded: Traded) = target handle traded

        override def handle(cancelled: Cancelled) = target handle cancelled

        override def handle(completed: Completed) = target handle completed
    }

    case class TradeDone(price: SignedTicks, volume: Quantity)

    trait AbstractOrderQueue[Currency] {
        val priceLevels: marketsim.reactive.Signal[List[(SignedTicks, Quantity)]]

        val tradeDone = new marketsim.reactive.Event[TradeDone]
    }

    trait AbstractOrderBook[Currency] {
        val Asks: AbstractOrderQueue[Currency]
        val Bids: AbstractOrderQueue[Currency]
        val tickMapper: TickMapper[Currency]

        def process(order: LimitOrder)

        def process(order: MarketOrder)

        def cancel(token: AbstractCanceller, amountToCancel: Quantity)

        def fetchPriceLevelsTillVolume(limitVolume: Quantity)

        def cancellationToken : AbstractCanceller
    }

    case class MarketOrder(side: Side, volume: Quantity, sender: OrderListener)

    // TODO: introduce id
    case class LimitOrder(price: SignedTicks,
                          volume: Quantity,
                          sender: OrderListener,
                          cancellationKey: Option[linear.Canceller] = None)
    {
        def side = price.side
    }


    case class LimitOrderInfo(side: Side, price: SignedTicks, unmatchedVolume: Quantity, sender: OrderListener)

}

