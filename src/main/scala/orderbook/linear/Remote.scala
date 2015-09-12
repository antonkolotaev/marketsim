package orderbook.linear

import core.Scheduler

object Remote {

    class Queue[Currency](target : AbstractOrderQueue[Currency], dt : core.Duration)
        extends AbstractOrderQueue[Currency]
    {
        val bestPrice = ops.delay(dt) { target.bestPrice }
        val bestPriceVolume = ops.delay(dt) { target.bestPriceVolume }
        val lastTrade = ops.delay(dt) { target.lastTrade }
        val lastTrades = ops.delay(dt) { target.lastTrades }
        val priceLevels = ops.delay(dt) { target.priceLevels }
    }
    
    class DelayedOrderListener(original : OrderListener, dt : core.Duration) extends OrderListener
    {
        private def delay(whatToDo : => Unit) =
            Scheduler.after(dt) { whatToDo }
        
        override def traded(price : SignedTicks, amount : Quantity) = delay { original.traded(price, amount) }
        override def cancelled(amount : Quantity) = delay { original cancelled amount }
        override def completed() = delay { original completed () }
    }
    
    class Book[Currency](target   : AbstractOrderBook[Currency],
                         toBook   : core.Duration,
                         fromBook : core.Duration)
        extends AbstractOrderBook[Currency]
    {
        val Asks = new Queue(target.Asks, fromBook)
        val Bids = new Queue(target.Bids, fromBook)

        def queue(side : Side) = side match {
            case Sell => Asks
            case Buy => Bids
        }

        val tickMapper = target.tickMapper
        
        private def delay(whatToDo : => Unit) = 
            Scheduler.after(toBook) { whatToDo }
        
        def cancel(token: Canceller, amountToCancel: Quantity) = delay { target.cancel(token, amountToCancel) }
        def process(order: LimitOrder) = delay { target process order.copy(sender = new DelayedOrderListener(order.sender, fromBook)) }
        def process(order: MarketOrder) = delay { target process order.copy(sender = new DelayedOrderListener(order.sender, fromBook)) }

        def fetchPriceLevelsTillVolume(user : AnyRef, limitVolume : Quantity) = target fetchPriceLevelsTillVolume (user, limitVolume)
    }

}
