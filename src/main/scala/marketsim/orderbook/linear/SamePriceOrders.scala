package marketsim
package orderbook
package linear

/**
 * Represents a queue of limit orders sharing the same price
 * It keeps track of cumulative volume of orders kept
 */
class SamePriceOrders(val price: SignedTicks) {
    private val entries_ = collection.mutable.Queue.empty[Entry]
    private var totalVolume_ : Quantity = 0

    /**
     * @return cumulative unmatched volume of orders kept in the level
     */
    def totalVolume = totalVolume_

    /**
     * Side of orders kept in the queue
     */
    val side = price.side

    /**
     * @return description for orders kept in the queue. used for debugging purposes
     */
    def ownOrders = entries_ map {
        _.createInfo
    }

    override def toString = s"[$price : ${
        entries_ map {
            _.unmatchedVolume
        } mkString " "
    }]"

    /**
     * Stores the order in the queue.
     * Updates cumulative volume of orders
     * @param unmatchedVolume -- size of order to be stored
     * @return -- order cancellation token
     */
    protected[linear] def storeImpl(order : LimitOrder, unmatchedVolume: Quantity) = {
        val e = new Entry(order, unmatchedVolume)
        entries_ enqueue e
        totalVolume_ += unmatchedVolume
        order.cancellationKey foreach {
            _ set(e, this)
        }
    }

    private[linear] def cancel(e: Entry, amountToCancel: Quantity) = {
        val cancelled = e cancel amountToCancel
        totalVolume_ -= cancelled
        cancelled
    }

    /**
     * Matches orders kept in the queue with an incoming order
     * Fires 'traded' event for our and the incoming order
     * Fires 'completed' event if our order is completely matched
     * @param volume -- size of incoming order
     * @param sender -- order event listener for the incoming order
     * @return unmatched volume of the incoming order
     */
    protected[linear] def matchImpl(volume: Quantity, sender: OrderBase) = {
        var unmatched = volume
        while (unmatched > 0 && entries_.nonEmpty) {
            val e = entries_.head
            val traded = e matchWith(unmatched, sender)
            unmatched -= traded
            totalVolume_ -= traded
            if (e.fulfilled)
                entries_.dequeue()
        }
        unmatched
    }
}

