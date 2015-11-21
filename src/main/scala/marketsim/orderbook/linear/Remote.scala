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

    class DelayedOrderListener(original: OrderListener, fromBook: Duration) extends OrderListener {
        private def delay(whatToDo: => Unit) = Remote.delay(fromBook) {
            whatToDo
        }

        override def handle(traded: Traded) = delay {
            original handle traded
        }

        override def handle(cancelled: Cancelled) = delay {
            original handle cancelled
        }

        override def handle(completed: Completed) = delay {
            original handle completed
        }
    }

    private val delayedListeners = collection.mutable.Map.empty[(OrderListener, Duration), OrderListener]

    private def delayedOrderListener(original: OrderListener, dt: Duration) =
        delayedListeners getOrElseUpdate((original, dt), new DelayedOrderListener(original, dt))

    /*private[linear]*/ def recreateDelayedListeners() = delayedListeners.clear()

    private[linear] def delayedListenersCount = delayedListeners.size

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
            target process order.copy(sender = delayedOrderListener(order.sender, fromBook))
        }

        def process(order: MarketOrder) = delay {
            target process order.copy(sender = delayedOrderListener(order.sender, fromBook))
        }

        def fetchPriceLevelsTillVolume(limitVolume: Quantity) = delay {
            target fetchPriceLevelsTillVolume limitVolume
        }
    }

}

