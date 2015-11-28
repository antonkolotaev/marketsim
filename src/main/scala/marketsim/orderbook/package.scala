package marketsim

import marketsim.core.Scheduler

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

    trait AbstractOrderQueue[Currency] {
        val priceLevels: marketsim.reactive.Signal[List[(Ticks, Currency, Quantity)]]

        val tradeDone = new marketsim.reactive.Event[TradeDone]
    }

    case class BestPrice[Currency](queue: AbstractOrderQueue[Currency])
        extends marketsim.reactive.UnaryBase(queue.priceLevels, Option.empty[Ticks], s"BestPrice($queue)") {
        def F(a: List[(Ticks, Currency, Quantity)]) = a.headOption map {
            _._1
        }
    }

    case class BestPriceCurrency[Currency](queue: AbstractOrderQueue[Currency])
        extends marketsim.reactive.UnaryBase(queue.priceLevels, Option.empty[Currency], s"BestPriceCurrency($queue)") {
        def F(a: List[(Ticks, Currency, Quantity)]) = a.headOption map {
            _._2
        }
    }

    case class BestPriceVolume[Currency](queue: AbstractOrderQueue[Currency])
        extends marketsim.reactive.UnaryBase(queue.priceLevels, Option.empty[Quantity], s"BestPriceVolume($queue)") {
        def F(a: List[(Ticks, Currency, Quantity)]) = a.headOption map {
            _._3
        }
    }

    trait AbstractOrderBook[Currency] {
        val Asks: OrderQueue
        val Bids: OrderQueue

        val tickMapper: TickMapper[Currency]

        def process(order: LimitOrder)

        def process(order: MarketOrder)

        type CancellationToken
        type OrderQueue <: AbstractOrderQueue[Currency]

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

