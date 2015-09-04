package orderbook.linear

/**
 * Represents a queue of orders of the same price as a node of a double-linked list
 * We use the double-linked list in assumption that orders are placed into the queue around the best price
 * We use explicit double-linked representation in order to have a fast navigation
 * to the previous/next price level which will be used by observables like CumulativePrice(from_volume, to_volume)
 * @param price -- price shared by all limit orders kept in the queue
 * @param prev -- optional reference to the previous price level (with more aggressive price)
 * @param next -- optional reference to the next price level (with less aggressive price)
 *             we suppose that the last valid level references to an level with infinite price =>
 *                  we need to make one comparison less
 */
class PriceLevel(price : SignedTicks,
                 private var prev : Option[PriceLevel],
                 private var next : Option[PriceLevel])
  extends SamePriceOrders(price)
{
    // at first we register this node in the previous and the next nodes
    prev foreach { _.next = Some(this) }
    next foreach { _.prev = Some(this) }

    def getPrevious = prev
    def getNext = next

    def store(order : LimitOrder, sender : OrderListener) : Quantity => Quantity =

        if (order.price >= next.get.price) // we assume that an order with infinite price ends the queue
            next.get store (order, sender)
        else
            (if (order.price == price)  this else
            /*  order.price > price */ new PriceLevel(order.price, Some(this), next)
                ) storeImpl (order.volume, sender)

    def allOrders : Iterable[LimitOrderInfo] = ownOrders ++ (next map { _.allOrders } getOrElse Nil)
}
