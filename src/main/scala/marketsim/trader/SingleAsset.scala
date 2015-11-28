package marketsim
package trader
/*
class SingleAsset(val book : orderbook.AbstractOrderBook) extends orderbook.OrderListener
{
    type Currency = book.Currency
    /**
     * Number of assets owned by the trader at the moment
     */
    val inventory = new Variable(0, s"Inventory($this)")

    /**
     * Position of the trader at market: positive if sells, negative if buys
     */
    val position = new Variable(0, s"Position($this)")

    val balance = new Variable(cents(0), s"Balance($this)")

    //val bestLevelPrice = marketsim.reactive.IfThenElse()

    //val balanceClearedAtBestLevel =

    private def commit() = {
        inventory commit()
        balance commit()
        position commit()
    }

    def adjustPositionAndVolume(side : Side, volume : Quantity) = {
        position setAndCommit (position() + (if (side == Sell) -volume else volume))
        book fetchPriceLevelsTillVolume position() + inventory()
    }

    def sendMarket(side : Side, volume : Quantity) = {
        adjustPositionAndVolume(side, volume)
        book process new book.MarketOrder(side, volume, this)
    }

    def sendLimit(side : Side, price : USD, volume : Quantity, canceller : Option[orderbook.AbstractOrderBook[USD]#CancellationToken] = None) = {
        adjustPositionAndVolume(side, volume)
        val ticks = book.tickMapper toTicks (price, side)
        book process new book.LimitOrder(side, ticks, volume, this, canceller)
    }

    def sendCancellableLimit(side : Side, price : USD, volume : Quantity) = {
        val canceller = book.cancellationToken
        adjustPositionAndVolume(side, volume)
        val ticks = book.tickMapper toTicks (price, side)
        book process new book.LimitOrder(side, ticks, volume, this, canceller)
        canceller
    }
    override def handle(traded : orderbook.Traded) = {
        val priceInCurrency = book.tickMapper toCurrency traded.price.ticks
        traded.side match {
            case Buy =>
                inventory setWithoutCommit (inventory() + traded.volume)
                position setWithoutCommit (position() - traded.volume)
                balance setWithoutCommit(balance() - priceInCurrency * traded.volume)
            case Sell =>
                inventory setWithoutCommit (inventory() - traded.volume)
                position setWithoutCommit (position() + traded.volume)
                balance setWithoutCommit(balance() + priceInCurrency * traded.volume)
        }
        core.Scheduler.asyncAgain { commit() }
    }

    override def handle(cancelled : orderbook.Cancelled) = {
        position setWithoutCommit (position() +
            (if (cancelled.side == Buy) -cancelled.amount else +cancelled.amount))
    }

    override def handle(completed: orderbook.Completed) {}
}
*/
