package orderbook.linear

class Entry(private var unmatched : Quantity,
            val sender : OrderListener)
{
    def unmatchedVolume = unmatched

    def createInfo(side : Side, price : SignedTicks) =
        LimitOrderInfo(side, price, unmatched, sender)

    def cancel(amount : Quantity) =
    {
        val toCancel = amount min unmatchedVolume
        unmatched -= toCancel
        sender cancelled toCancel
        if (unmatched == 0)
            sender completed ()
        toCancel
    }
}
