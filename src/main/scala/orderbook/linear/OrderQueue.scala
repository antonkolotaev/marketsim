package orderbook.linear

class OrderQueue(side : Side)
{
    private var bestPriceLevel = new PriceLevel(Int.MaxValue, None, None)

    def store(order : LimitOrder, sender : OrderListener) = {
        if (order.price < bestPriceLevel.price)
            bestPriceLevel = new PriceLevel(order.price, None, Some(bestPriceLevel))
        bestPriceLevel store(order, sender)
    }

    def bestLevel = bestPriceLevel

    def allOrders = bestPriceLevel.allOrders
}
