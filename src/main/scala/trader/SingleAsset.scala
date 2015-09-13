package trader

import reactive.Variable
import orderbook.linear._

class SingleAsset(val book : AbstractOrderBook[USD]) extends OrderListener
{
    /**
     * Number of assets owned by the trader at the moment
     */
    val inventory = new Variable(0)

    /**
     * Position of the trader at market: positive if sells, negative if buys
     */
    val position = new Variable(0)

    val balance = new Variable(cents(0))

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
        book process new MarketOrder(side, volume, this)
    }

    def sendLimit(side : Side, price : USD, volume : Quantity, canceller : Option[Canceller] = None) = {
        adjustPositionAndVolume(side, volume)
        val ticks = book.tickMapper toTicks (price, side)
        book process new LimitOrder(side, ticks, volume, this, canceller)
    }

    def sendCancellableLimit(side : Side, price : USD, volume : Quantity) = {
        val canceller = new Canceller
        sendLimit(side, price, volume, Some(canceller))
        canceller
    }

    override def handle(traded : Traded) = {
        val priceInCurrency = book.tickMapper toCurrency traded.price.ticks
        traded.price.side match {
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

    override def handle(cancelled : Cancelled) = {

    }

    override def handle(completed: Completed) {}
}
