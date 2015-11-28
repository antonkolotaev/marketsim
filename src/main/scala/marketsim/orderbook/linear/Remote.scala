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

    class Queue[Currency](target: AbstractOrderQueue[Currency], fromBook: Duration)
        extends AbstractOrderQueue[Currency] {
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

    class Book[Currency](val target: AbstractOrderBook[Currency],
                         toBook: Duration,
                         fromBook: Duration)
        extends AbstractOrderBook[Currency] {
        val Asks = new Queue(target.Asks, fromBook)
        val Bids = new Queue(target.Bids, fromBook)

        def queue(side: Side) = side match {
            case Sell => Asks
            case Buy => Bids
        }

        type CancellationToken = target.CancellationToken

        val tickMapper = target.tickMapper

        private def delay(whatToDo: => Unit) =
            Scheduler.after(toBook) {
                whatToDo
            }

        def cancellationToken = target.cancellationToken

        def cancel(token: CancellationToken, amountToCancel: Quantity) = delay {
            target.cancel(token, amountToCancel)
        }

        def process(order: LimitOrder) = delay {
            val delayed = order.copy(sender = delayedOrderListener(order.sender, fromBook))
            target process delayed.asInstanceOf[target.LimitOrder]
        }

        def process(order: MarketOrder) = delay {
            val delayed = order.copy(sender = delayedOrderListener(order.sender, fromBook))
            target process delayed.asInstanceOf[target.MarketOrder]
        }

        def fetchPriceLevelsTillVolume(limitVolume: Quantity) = delay {
            target fetchPriceLevelsTillVolume limitVolume
        }
    }

}

