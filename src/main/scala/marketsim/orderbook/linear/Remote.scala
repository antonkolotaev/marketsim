package marketsim
package orderbook
package linear

import core.Scheduler
import ops._

object Remote {

    private def delay(dt: Duration)(whatToDo: => Unit) =
        Scheduler.afterAgain(dt) {
            whatToDo
        }

    class Queue(target: AbstractOrderQueue, fromBook: Duration)
        extends AbstractOrderQueue {
            val priceLevels = target.priceLevels delayed fromBook

            target.tradeDone += { trade => delay(fromBook) {
                tradeDone fire trade
            }
        }

    }

    class DelayedOrderListener(original: OrderBase, fromBook: Duration) extends OrderListener {
        private def delay(whatToDo: => Unit) = Remote.delay(fromBook) {
            whatToDo
        }

        override def handle(order : OrderBase, traded: Traded) = delay {
            original fire traded
        }

        override def handle(order : OrderBase, cancelled: Cancelled) = delay {
            original fire cancelled
        }

        override def handle(order : OrderBase, completed: Completed) = delay {
            original fire completed
        }
    }

    private def delayedOrderListener(original: OrderBase, dt: Duration) =
                                                new DelayedOrderListener(original, dt)

    class Book[Currency](target: AbstractOrderBook[Currency],
                         toBook: Duration,
                         fromBook: Duration)
        extends AbstractOrderBook[Currency] {
        val Asks = new Queue(target.Asks, fromBook)
        val Bids = new Queue(target.Bids, fromBook)

        def queue(side: Side) = side match {
            case Sell => Asks
            case Buy => Bids
        }

        val tickMapper = target.tickMapper

        private def delay(whatToDo: => Unit) =
            Scheduler.after(toBook) {
                whatToDo
            }

        def cancellationToken = target.cancellationToken

        def cancel(token: AbstractCanceller, amountToCancel: Quantity) = delay {
            target.cancel(token, amountToCancel)
        }

        def process(order: LimitOrder) = delay {
            target process order.copy(sender = delayedOrderListener(order, fromBook))
        }

        def process(order: MarketOrder) = delay {
            target process order.copy(sender = delayedOrderListener(order, fromBook))
        }

        def fetchPriceLevelsTillVolume(limitVolume: Quantity) = delay {
            target fetchPriceLevelsTillVolume limitVolume
        }
    }

}

