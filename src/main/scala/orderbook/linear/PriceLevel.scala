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

    /**
     * Stores a limit order in the queue
     * @param order -- order to keep
     * @param sender -- order events
     * @return -- cancellation token: a functional object that can be used to cancel a part of the order
     */
    def store(order : LimitOrder, sender : OrderListener) : Quantity => Quantity =

        if (order.price >= next.get.price) // we assume that an order with infinite price ends the queue
            next.get store (order, sender)
        else
            (if (order.price == price)  this else
            /*  order.price > price */ new PriceLevel(order.price, Some(this), next)
                ) storeImpl (order.volume, sender)

    /**
     * Matches with a limit order (volume, limitPrice, sender)
     * Fires traded event for our orders and for the incoming one
     * Fires completed event for our orders that were fulfilled
     * @param volume -- volume of the incoming order
     * @param limitPrice -- limit price of the incoming order (+inf in case of market order)
     * @param sender -- events for the incoming order
     * @return -- unmatched volume of the incoming order
     */
    def matchWith(volume : Quantity, limitPrice : SignedTicks, sender : OrderListener) : Quantity =
        
        if (limitPrice < price)
            volume
        else
            matchImpl(volume, sender) match {
                case 0 => 0
                case unmatched => next.get matchWith (unmatched, limitPrice, sender)
            }

    def allOrders : Iterable[LimitOrderInfo] = ownOrders ++ (next map { _.allOrders } getOrElse Nil)
}
