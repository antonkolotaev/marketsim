package marketsim
package orderbook
package linear

/**
 * Represents a limit order stored in an order book
 * We store for it unmatched volume and its events listener
 * @param unmatched -- initial unmatched order volume
 * @param order -- order stored
 */
private[linear] class Entry(val order : LimitOrder,
                            private var unmatched: Quantity) {
    /**
     * @return Current unmatched volume
     */
    def unmatchedVolume = unmatched

    def createInfo =
        LimitOrderInfo(order.side, order.price, unmatched, order.sender)

    /**
     * @return true iff the order is completely matched or cancelled
     */
    def fulfilled = unmatchedVolume == 0

    /**
     * cancels 'amount' from unmatched order volume
     * fires 'cancelled' event and if the order gets completely matched fires 'completed'
     * @param amount -- amount to cancel
     * @return actually cancelled order volume
     */
    def cancel(amount: Quantity) = {
        val toCancel = amount min unmatchedVolume
        unmatched -= toCancel
        order fire Cancelled(order.side, toCancel)
        if (unmatched == 0)
            order fire Completed()
        toCancel
    }

    /**
     * Matches the order with incoming order
     * fires 'traded' event and if the order gets completely traded fires 'completed'
     * @param amount -- incoming order size
     * @param incoming_sender -- incoming order events; 'traded' event will be fired,
     *                        it is responsibility of the caller to call properly 'completed'
     * @return actually traded order volume
     */
    def matchWith(amount: Quantity, incoming_sender: OrderBase) = {
        val toTrade = amount min unmatchedVolume
        unmatched -= toTrade
        order fire Traded(order.price, toTrade)
        incoming_sender fire Traded(order.price.opposite, toTrade)
        if (unmatched == 0)
            order fire Completed()
        toTrade
    }

    override def toString = s"Entry($unmatchedVolume)"
}

