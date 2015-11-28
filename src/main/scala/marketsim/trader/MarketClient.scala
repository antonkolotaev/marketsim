package marketsim
package trader

import marketsim.orderbook._
import marketsim.reactive.Event

class MarketClient(val orderbook : AbstractOrderBook) extends OrderListener
{
    self =>

    val onTraded = new Event[Traded]
    val onCancelled = new Event[Cancelled]

    type LimitOrder = orderbook.LimitOrder
    type MarketOrder = orderbook.MarketOrder

    val orderSent = new Event[AbstractOrder]
    val orderTraded = new Event[(AbstractOrder, Traded)]
    val orderCancelled = new Event[(AbstractOrder, Quantity)]
    val orderCompleted = new Event[AbstractOrder]

    def sendMarketOrder(side : Side, volume : Quantity): Unit = {
        val listener =
            if (hasOrderSubscriptions)
                new InstanceListener
            else
                this
        val order = orderbook.MarketOrder(side, volume, listener)
        listener match {
            case instance : InstanceListener => instance.order = order
            case _ =>
        }
        orderbook process order
        orderSent fire order
    }

    def sendLimitOrder(side : Side, volume : Quantity, ticks : Ticks, cancellable : Boolean): Unit = {
        val listener =
            if (hasOrderSubscriptions)
                new InstanceListener
            else
                this
        val cancellationToken = if (cancellable) Some(orderbook.cancellationToken) else None
        val order = orderbook.LimitOrder(side, ticks, volume, listener, cancellationToken)
        listener match {
            case instance : InstanceListener => instance.order = order
            case _ =>
        }
        orderbook process order
        orderSent fire order
    }

    private def hasOrderSubscriptions = orderTraded.nonEmpty || orderCancelled.nonEmpty || orderCompleted.nonEmpty

    class InstanceListener extends OrderListener
    {
        private[MarketClient] var order : AbstractOrder = null

        override def handle(traded: Traded) = {
            self.orderTraded fire (order, traded)
            self handle traded
        }

        override def handle(cancelled: Cancelled) = {
            self.orderCancelled fire (order, cancelled.amount)
            self handle cancelled
        }

        override def handle(completed: Completed) = {
            self.orderCompleted fire order
            self handle completed
        }
    }

    override def handle(traded: Traded) = onTraded fire traded

    override def handle(cancelled: Cancelled) = onCancelled fire cancelled

    override def handle(completed: Completed) {}

    private var delayedCache = Map.empty[Duration, OrderListener]

    override def delayed(dt : Duration) = {
        if (!(delayedCache contains dt))
            delayedCache = delayedCache updated (dt, super.delayed(dt))
        delayedCache(dt)
    }

}
