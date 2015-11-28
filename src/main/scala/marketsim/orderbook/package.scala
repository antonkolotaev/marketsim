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
        def handle(order : OrderBase, traded: Traded)
        def handle(order : OrderBase, cancelled: Cancelled)
        def handle(order : OrderBase, completed: Completed)
    }

    case class TradeDone(price: SignedTicks, volume: Quantity)

    trait AbstractOrderQueue{
        val priceLevels: marketsim.reactive.Signal[List[(SignedTicks, Quantity)]]

        val tradeDone = new marketsim.reactive.Event[TradeDone]
    }

    trait AbstractOrderBook[Currency] {
        val Asks: AbstractOrderQueue
        val Bids: AbstractOrderQueue
        val tickMapper: TickMapper[Currency]

        def process(order: LimitOrder)

        def process(order: MarketOrder)

        def cancel(token: AbstractCanceller, amountToCancel: Quantity)

        def fetchPriceLevelsTillVolume(limitVolume: Quantity)

        def cancellationToken : AbstractCanceller
    }

    trait OrderBase
    {
        def fire(traded: Traded)
        def fire(cancelled: Cancelled)
        def fire(completed: Completed)

        def side : Side
        def volume : Quantity
    }

    case class MarketOrder(side: Side, volume: Quantity, sender: OrderListener) extends OrderBase
    {
        def fire(traded: Traded) = sender handle (this, traded)
        def fire(cancelled: Cancelled) = sender handle (this, cancelled)
        def fire(completed: Completed) = sender handle (this, completed)
    }

    // TODO: introduce id
    case class LimitOrder(price: SignedTicks,
                          volume: Quantity,
                          sender: OrderListener,
                          cancellationKey: Option[linear.Canceller] = None) extends OrderBase
    {
        def side = price.side
        def fire(traded: Traded) = sender handle (this, traded)
        def fire(cancelled: Cancelled) = sender handle (this, cancelled)
        def fire(completed: Completed) = sender handle (this, completed)
    }


    case class LimitOrderInfo(side: Side, price: SignedTicks, unmatchedVolume: Quantity, sender: OrderListener)

}

