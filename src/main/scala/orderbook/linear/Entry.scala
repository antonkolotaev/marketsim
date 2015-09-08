package orderbook.linear

/**
 * Represents a limit order stored in an order book
 * We store for it unmatched volume and its events listener
 * @param unmatched -- initial unmatched order volume
 * @param sender -- order events listener
 * Note that side and price of the order is kept in PriceLevel
 */
private[linear] class Entry(private var unmatched : Quantity,
                                    val sender : OrderListener)
{
    /**
     * @return Current unmatched volume
     */
    def unmatchedVolume = unmatched

    /**
     * Creates a limit order description
     * @param side -- order side passed from PriceLevel
     * @param price -- order price passed from PriceLevel
     */
    def createInfo(side : Side, price : SignedTicks) =
        LimitOrderInfo(side, price, unmatched, sender)

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
    def cancel(amount : Quantity) =
    {
        val toCancel = amount min unmatchedVolume
        unmatched -= toCancel
        sender cancelled toCancel
        if (unmatched == 0)
            sender completed ()
        toCancel
    }

    /**
     * Matches the order with incoming order
     * fires 'traded' event and if the order gets completely traded fires 'completed'
     * @param ourPrice -- price of the entry passed from PriceLevel, used for 'traded' event
     * @param amount -- incoming order size
     * @param incoming_sender -- incoming order events; 'traded' event will be fired,
     *                        it is responsibility of the caller to call properly 'completed'
     * @return actually traded order volume
     */
    def matchWith(ourPrice : SignedTicks, amount : Quantity, incoming_sender : OrderListener) = {
        val toTrade = amount min unmatchedVolume
        unmatched -= toTrade
        sender traded (ourPrice.ticks, toTrade)
        incoming_sender traded (ourPrice.ticks, toTrade)
        if (unmatched == 0)
            sender completed()
        toTrade
    }

    override def toString = s"Entry($unmatchedVolume)"
}
