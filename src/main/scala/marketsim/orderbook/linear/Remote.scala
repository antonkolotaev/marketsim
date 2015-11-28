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

    private def delayedOrderListener(original: OrderListener, dt: Duration) = original delayed dt

    class Book[Currency](val target: AbstractOrderBook[Currency],
                         toBook: Duration,
                         fromBook: Duration)
        extends AbstractOrderBook[Currency] {
        val Asks = new OrderQueue(target.Asks, fromBook)
        val Bids = new OrderQueue(target.Bids, fromBook)

        def queue(side: Side) = side match {
            case Sell => Asks
            case Buy => Bids
        }

        type CancellationToken = target.CancellationToken
        type OrderQueue = Queue[Currency]

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

