package orderbook.linear

import core.Scheduler

object Remote {

    class Queue(target : AbstractOrderQueue, dt : core.Duration) extends AbstractOrderQueue {
        val bestPrice = ops.delay(dt) { target.bestPrice }
        val bestPriceVolume = ops.delay(dt) { target.bestPriceVolume }
        val lastTrade = ops.delay(dt) { target.lastTrade }
        val lastTrades = ops.delay(dt) { target.lastTrades }
    }
    
    class DelayedOrderListener(original : OrderListener, dt : core.Duration) extends OrderListener
    {
        private def delay(whatToDo : => Unit) =
            Scheduler.after(dt) { whatToDo }
        
        override def traded(price : Ticks, amount : Quantity) = delay { original.traded(price, amount) }
        override def cancelled(amount : Quantity) = delay { original cancelled amount }
        override def completed() = delay { original completed () }
    }
    
    class Book(target : AbstractOrderBook, toBook : core.Duration, fromBook : core.Duration) extends AbstractOrderBook {
        val Asks = new Queue(target.Asks, fromBook)
        val Bids = new Queue(target.Bids, fromBook)
        
        private def delay(whatToDo : => Unit) = 
            Scheduler.after(toBook) { whatToDo }
        
        def cancel(token: Canceller, amountToCancel: Quantity) = delay { target.cancel(token, amountToCancel) }
        def process(order: LimitOrder) = delay { order.copy(sender = new DelayedOrderListener(order.sender, fromBook)) }
        def process(order: MarketOrder) = delay { order.copy(sender = new DelayedOrderListener(order.sender, fromBook)) }

    }

}
