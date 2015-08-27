package orderbook.linear

private[linear] class Entry(private var unmatched : Quantity,
                                    val sender : OrderListener)
{
    def unmatchedVolume = unmatched

    def createInfo(side : Side, price : SignedTicks) =
        LimitOrderInfo(side, price, unmatched, sender)

    def fulfilled = unmatchedVolume == 0

    def cancel(amount : Quantity) =
    {
        val toCancel = amount min unmatchedVolume
        unmatched -= toCancel
        sender cancelled toCancel
        if (unmatched == 0)
            sender completed ()
        toCancel
    }

    def matchWith(ourPrice : SignedTicks, amount : Quantity, incoming_sender : OrderListener) = {
        val toTrade = amount min unmatchedVolume
        unmatched -= toTrade
        sender traded (ourPrice, toTrade)
        incoming_sender traded (ourPrice, toTrade)
        if (unmatched == 0)
            sender completed()
        toTrade
    }
}
