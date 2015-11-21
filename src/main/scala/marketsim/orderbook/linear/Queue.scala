package marketsim
package orderbook
package linear

/**
 * Represents a queue of limit orders of one side
 * @param side -- side of orders held in the queue
 */
class Queue[Currency](side : Side, infiniteCurrency : Currency) extends AbstractOrderQueue[Currency]
{
    // we are going to mark the end of the queue by a dummy order with infinite price
    private val terminal = new {
        val volume = 1
        val listener = new OrderListener {}
        val dummyOrder = new LimitOrder(TerminalOrderPrice, 1, listener, None)

        val level = new PriceLevel(TerminalOrderPrice, None, None)
        level storeImpl (dummyOrder, volume)

        val info = level.allOrders.head
    }

    /**
     * Reference to the best price level
     */
    private var bestPriceLevel = terminal.level

    /**
     * Stores a limit order in the queue
     * @param volume -- volume of an order to keep
     * @param order -- order to keep
     * @return -- cancellation token: a functional object that can be used to cancel a part of the order
     */
    private[linear] def store(order          : LimitOrder,
                              volume         : Quantity) =
    {
        if (order.price.isMoreAggressiveThan(bestPriceLevel.price)) {
            bestPriceLevel = new PriceLevel(order.price, None, Some(bestPriceLevel))
        }
        bestPriceLevel store(order, volume)
        validateBestPrice()
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
        val proxyEvents = new OrderListenerProxy(sender) {
            override def handle(traded : Traded) = {
                super.handle(traded)
                tradeDone fire TradeDone(traded.price.opposite, traded.volume)
            }
        }
        val unmatched = bestPriceLevel matchWith (limitPrice, volume, proxyEvents)
        removeEmptyBestLevels()
        unmatched
    }

    private[linear] def cancel(token : Canceller, amount : Quantity) = {
        token(amount)
        removeEmptyBestLevels()
    }

    /**
     * Removes all empty price levels from the head of the queue
     */
    private[linear] def removeEmptyBestLevels() = {
        while (bestPriceLevel.totalVolume == 0) {
            bestPriceLevel = bestPriceLevel.dispose().get
        }
        validateBestPrice()
    }

    /**
     * @return the best non-empty price level
     */
    def bestLevel = bestPriceLevel

    def allOrders = bestPriceLevel.allOrders takeWhile (_ != terminal.info)

    val priceLevels = new marketsim.reactive.Variable[List[(SignedTicks, Quantity)]](Nil, s"PriceLevels($this)")

    def updatePriceLevels() = {
        val levels = bestPriceLevel levelsTill priceLevelToFetch
        //println(s"T = ${marketsim.core.Scheduler.currentTime}; $priceLevels <- $levels")
        priceLevels set levels
    }

    private var priceLevelToFetch = 0

    def fetchPriceLevelsTillVolume(limitVolume : Quantity) = {
        priceLevelToFetch = priceLevelToFetch max limitVolume
    }

    override def toString = side match {
        case Buy => "Bids"
        case Sell => "Asks"
    }


    private def validateBestPrice(): Unit = {
        updatePriceLevels()
    }
}



