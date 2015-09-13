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

    override def traded(price : SignedTicks, amount : Quantity) = {
        val priceInCurrency = book.tickMapper toCurrency price.ticks
        price.side match {
            case Buy =>
                inventory setWithoutCommit (inventory() + amount)
                balance setWithoutCommit(balance() - priceInCurrency * amount)
            case Sell =>
                inventory setWithoutCommit (inventory() - amount)
                balance setWithoutCommit(balance() + priceInCurrency * amount)
        }
        core.Scheduler.asyncAgain { commit() }
    }
    override def cancelled(amount : Quantity) {}
    override def completed() {}
}
