package orderbook.linear

class SamePriceOrders {
    private val entries_ = collection.mutable.Queue.empty[Entry]
    private var totalVolume_ : Quantity = 0

    protected def storeImpl(volume : Quantity, sender : OrderListener) =
    {
        val e = new Entry(volume, sender)
        entries_ enqueue e
        totalVolume_ += volume
        (amountToCancel : Quantity) => totalVolume_ -= e cancel amountToCancel
    }

    def totalVolume = totalVolume_
    protected def entries = entries_
}
