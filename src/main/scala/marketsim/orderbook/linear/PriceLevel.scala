package marketsim.orderbook.linear

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
class PriceLevel[Currency](price : SignedTicks, val priceInCurrency : Currency,
                           private var prev : Option[PriceLevel[Currency]],
                           private var next : Option[PriceLevel[Currency]])
  extends SamePriceOrders(price)
{
    // at first we register this node in the previous and the next nodes
    prev foreach { _.next = Some(this) }
    next foreach { _.prev = Some(this) }

    def dispose() = {
        prev foreach { _.next = next }
        next foreach { _.prev = prev }
        next
    }

    def getPrevious = prev
    def getNext = next

    def levels : List[PriceLevel[Currency]] = this :: {next map { _.levels } getOrElse Nil}

    def levelsTill(volumeToFetch : Quantity) : List[(Ticks, Currency, Quantity)] =
        if (volumeToFetch > 0 && next.nonEmpty)
            (price.ticks, priceInCurrency, totalVolume min volumeToFetch) ::
                { next.get levelsTill (volumeToFetch - totalVolume) }
        else
            Nil

    /**
     * Stores a limit order in the queue
     * @param price -- price of an order to keep
     * @param volume -- volume of an order to keep
     * @param sender -- order events
     * @return -- cancellation token: a functional object that can be used to cancel a part of the order
     */
    def store(price : SignedTicks, priceInCurrency : Currency, volume : Quantity, sender : OrderListener, cancellationKey : Option[Canceller]) : Unit =

        if (!price.isMoreAggressiveThan(next.get.price)) // we assume that an order with infinite price ends the queue
            next.get store (price, priceInCurrency, volume, sender, cancellationKey)
        else
            (if (price == this.price)  this else
            /*  order.price > price */ new PriceLevel(price, priceInCurrency, Some(this), next)
                ) storeImpl (volume, sender, cancellationKey)

    /**
     * Matches with a limit order (volume, limitPrice, sender)
     * Fires traded event for our orders and for the incoming one
     * Fires completed event for our orders that were fulfilled
     * @param limitPrice -- limit price of the incoming order (+inf in case of market order)
     * @param volume -- volume of the incoming order
     * @param sender -- events for the incoming order
     * @return -- unmatched volume of the incoming order
     */
    def matchWith(limitPrice : SignedTicks, volume : Quantity, sender : OrderListener) : Quantity =
        
        if (limitPrice isMoreAggressiveThan price)
            volume
        else
            matchImpl(volume, sender) match {
                case 0 => 0
                case unmatched => next.get matchWith (limitPrice, unmatched, sender)
            }

    def allOrders : Iterable[LimitOrderInfo] = ownOrders ++ (next map { _.allOrders } getOrElse Nil)
}
