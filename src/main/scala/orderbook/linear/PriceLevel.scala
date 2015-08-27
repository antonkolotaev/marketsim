package orderbook.linear

class PriceLevel(val price : SignedTicks,
                 private var prev : Option[PriceLevel],
                 private var next : Option[PriceLevel]) extends SamePriceOrders
{
    prev foreach { _.next = Some(this) }
    next foreach { _.prev = Some(this) }

    def getPrevious = prev
    def getNext = next

    def store(order : LimitOrder, sender : OrderListener) : Quantity => Unit =

        if (order.price >= next.get.price) // we assume that an order with infinite price ends the queue
            next.get store (order, sender)
        else
            (if (order.price == price)  this else
            /*  order.price > price */ new PriceLevel(order.price, Some(this), next)
                ) storeImpl (order.volume, sender)

    val side = Side of price

    def ownOrders = entries map { _ createInfo (side, price) }

    def allOrders : Iterable[LimitOrderInfo] = ownOrders ++ (next map { _.allOrders } getOrElse Nil)
}
