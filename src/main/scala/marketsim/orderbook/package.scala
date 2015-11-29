package marketsim

import marketsim.core.Scheduler

package object orderbook {

    trait TickMapper {
        def toTicks(x: Currency, side: Side): Ticks

        def toCurrency(x: Ticks): Currency
    }

    class LinearMapper(tickSize: Currency) extends TickMapper {

        def toTicks(x: Currency, side: Side) = side match {
            case Buy => Ticks(math.floor(x / tickSize).toInt)
            case Sell => Ticks(math.ceil(x / tickSize).toInt)
        }

        def toCurrency(x: Ticks) = tickSize * x.value
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
    trait OrderListener { self =>

        def handle(traded: Traded) {}

        def handle(cancelled: Cancelled) {}

        def handle(completed: Completed) {}

        def delayed(dt : Duration) : OrderListener = new OrderListener {

            private def delay(whatToDo : => Unit): Unit = {
                Scheduler.afterAgain(dt) {
                    whatToDo
                }
            }

            override def handle(traded: Traded): Unit = {
                delay {
                    self handle traded
                }
            }

            override def handle(traded: Cancelled): Unit = {
                delay {
                    self handle traded
                }
            }

            override def handle(traded: Completed): Unit = {
                delay {
                    self handle traded
                }
            }
        }
    }

    class OrderListenerProxy(target: OrderListener) extends OrderListener {
        override def handle(traded: Traded) = target handle traded

        override def handle(cancelled: Cancelled) = target handle cancelled

        override def handle(completed: Completed) = target handle completed
    }

    case class TradeDone(price: SignedTicks, volume: Quantity)

    trait AbstractOrderQueue {

        val priceLevels: reactive.Signal[List[(Ticks, Currency, Quantity)]]

        val tradeDone = new reactive.Event[TradeDone]
    }

    def BestPrice(queue : AbstractOrderQueue) =
        reactive.Unary(queue.priceLevels, s"$queue.BestPrice"){
            levels => levels.headOption map { _._1 }
        }

    def BestPriceCurrency(queue : AbstractOrderQueue) =
        reactive.Unary(queue.priceLevels, s"$queue.BestPriceCurrency"){
            levels => levels.headOption map { _._2 }
        }

    trait AbstractOrderBook {
        val Asks: OrderQueue
        val Bids: OrderQueue

        val tickMapper: TickMapper

        def process(order: LimitOrder)

        def process(order: MarketOrder)

        type CancellationToken
        type OrderQueue <: AbstractOrderQueue

        def cancel(token: CancellationToken, amountToCancel: Quantity)

        def fetchPriceLevelsTillVolume(limitVolume: Quantity)

        def cancellationToken : CancellationToken

        case class LimitOrder(side: Side,
                              price: Ticks,
                              volume: Quantity,
                              sender: OrderListener,
                              cancellationKey: Option[CancellationToken] = None) extends AbstractOrder

        case class MarketOrder(side: Side, volume: Quantity, sender: OrderListener) extends AbstractOrder
    }

    trait AbstractOrder {
        val side: Side
        val volume: Quantity
    }

    case class LimitOrderInfo(side: Side, price: SignedTicks, unmatchedVolume: Quantity, sender: OrderListener)

}

