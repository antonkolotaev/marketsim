package orderbook.linear

class SamePriceOrders {
    private val entries_ = collection.mutable.Queue.empty[Entry]
    private var totalVolume_ : Quantity = 0

    protected[linear] def storeImpl(volume : Quantity, sender : OrderListener) =
    {
        val e = new Entry(volume, sender)
        entries_ enqueue e
        totalVolume_ += volume
        (amountToCancel : Quantity) => totalVolume_ -= e cancel amountToCancel
    }

    protected[linear] def matchImpl(ourPrice : SignedTicks, volume : Quantity, sender : OrderListener) = {
        var unmatched = volume
        while (unmatched > 0 && entries_.nonEmpty) {
            val e = entries_.head
            val traded = e matchWith (ourPrice, unmatched, sender)
            unmatched -= traded
            totalVolume_ -= traded
            if (e.fulfilled)
                entries_.dequeue()
        }
        unmatched
    }

    def totalVolume = totalVolume_
    protected def entries = entries_
}
