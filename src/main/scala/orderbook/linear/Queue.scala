package orderbook.linear

import reactive.VariableOpt

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

        val level = new PriceLevel(TerminalOrderPrice, infiniteCurrency, None, None)
        level storeImpl (volume, listener, None)

        val info = level.allOrders.head
    }

    /**
     * Reference to the best price level
     */
    private var bestPriceLevel = terminal.level

    override def toString = bestPriceLevel.levels mkString "\n"

    /**
     * Stores a limit order in the queue
     * @param price -- price of an order to keep
     * @param volume -- volume of an order to keep
     * @param sender -- order events
     * @return -- cancellation token: a functional object that can be used to cancel a part of the order
     */
    private[linear] def store(price          : SignedTicks,
                              priceInCurrency: Currency,
                              volume         : Quantity,
                              sender         : OrderListener,
                              cancellationKey: Option[Canceller]) =
    {
        if (price isMoreAggressiveThan bestPriceLevel.price) {
            bestPriceLevel = new PriceLevel(price, priceInCurrency, None, Some(bestPriceLevel))
        }
        bestPriceLevel store(price, priceInCurrency, volume, sender, cancellationKey)
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
                lastTrade set Some(traded.price.ticks, traded.volume)
                lastTrades set (traded.price.ticks, traded.volume) :: lastTrades.value
            }
        }
        clearLastTrades()
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

    val bestPrice = new VariableOpt[Ticks]
    val bestPriceVolume = new VariableOpt[Quantity]
    val lastTrade = new VariableOpt[(Ticks, Quantity)]
    val lastTrades = new reactive.Variable[List[(Ticks, Quantity)]](Nil)
    val priceLevels = new reactive.Variable[List[(Ticks, Currency, Quantity)]](Nil)

    def updatePriceLevels() = {
        priceLevels set (bestPriceLevel levelsTill priceLevelToFetch)
    }

    private var priceLevelToFetch = 0

    def fetchPriceLevelsTillVolume(limitVolume : Quantity) = {
        priceLevelToFetch = priceLevelToFetch max limitVolume
    }


    private def validateBestPrice(): Unit = {
        if (bestPriceLevel == terminal.level) {
            bestPrice set None
            bestPriceVolume set None
        } else {
            bestPrice set Some(bestPriceLevel.price.ticks)
            bestPriceVolume set Some(bestPriceLevel.totalVolume)
        }
        updatePriceLevels()
    }

    private[linear] def clearLastTrades() = lastTrades set Nil
}



