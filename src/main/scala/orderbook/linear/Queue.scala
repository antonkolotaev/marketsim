package orderbook.linear

/**
 * Represents a queue of limit orders of one side
 * @param side -- side of orders held in the queue
 */
class Queue(side : Side)
{
    // we are going to mark the end of the queue by a dummy order with infinite price
    private val terminal = new {
        val volume = 1
        val listener = new OrderListener {}

        val level = new PriceLevel(TerminalOrderPrice, None, None)
        level storeImpl (volume, listener)

        val info = level.allOrders.head
    }

    /**
     * Reference to the best price level
     */
    private var bestPriceLevel = terminal.level

    /**
     * Stores a limit order in the queue
     * @param price -- price of an order to keep
     * @param volume -- volume of an order to keep
     * @param sender -- order events
     * @return -- cancellation token: a functional object that can be used to cancel a part of the order
     */
    private[linear] def store(price : SignedTicks, volume : Quantity, sender : OrderListener) = {
        if (price < bestPriceLevel.price)
            bestPriceLevel = new PriceLevel(price, None, Some(bestPriceLevel))
        bestPriceLevel store(price, volume, sender)
    }

    /**
     * Matches with a limit order (volume, limitPrice, sender)
     * Fires traded event for our orders and for the incoming one
     * Fires completed event for our orders that were fulfilled
     * @param limitPrice -- limit price of the incoming order (+inf in case of market order)
     * @param volume -- volume of the incoming order
     * @param sender -- events for the incoming order
     * @return -- unmatched volume of the incoming order
     */
    private[linear] def matchWith(limitPrice : SignedTicks, volume : Quantity, sender : OrderListener) : Quantity = {
        val unmatched = bestPriceLevel matchWith (limitPrice, volume, sender)
        removeEmptyBestLevels()
        unmatched
    }

    /**
     * Removes all empty price levels from the head of the queue
     */
    private def removeEmptyBestLevels() =
        while (bestPriceLevel.totalVolume == 0)
            bestPriceLevel = bestPriceLevel.dispose().get

    /**
     * @return the best non-empty price level
     */
    def bestLevel = {
        removeEmptyBestLevels()
        bestPriceLevel
    }

    def allOrders = bestPriceLevel.allOrders takeWhile (_ != terminal.info)
}


