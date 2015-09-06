package orderbook.linear

class OrderQueue(side : Side)
{
    private val terminalVolume = 1
    private val terminalListener = new OrderListener {}

    val terminalLevel = new PriceLevel(TerminalOrderPrice, None, None)
    terminalLevel storeImpl (terminalVolume, terminalListener)

    val terminalInfo = terminalLevel.allOrders.head

    private var bestPriceLevel = terminalLevel

    def store(order : LimitOrder, sender : OrderListener) = {
        if (order.price < bestPriceLevel.price)
            bestPriceLevel = new PriceLevel(order.price, None, Some(bestPriceLevel))
        bestPriceLevel store(order, sender)
    }

    def matchWith(volume : Quantity, limitPrice : SignedTicks, sender : OrderListener) : Quantity = {
        val unmatched = bestPriceLevel matchWith (volume, limitPrice, sender)
        while (bestPriceLevel.totalVolume == 0)
            bestPriceLevel = bestPriceLevel.dispose().get
        unmatched
    }

    def bestLevel = bestPriceLevel

    def allOrders = bestPriceLevel.allOrders takeWhile (_ != terminalInfo)
}


