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

    def sendMarket(side : Side, volume : Quantity) = {
        val order = new MarketOrder(side, volume, this)
        position setAndCommit (position() + (if (side == Sell) volume else -volume))
        book fetchPriceLevelsTillVolume (this, position())
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
        commit()
    }
    override def cancelled(amount : Quantity) {}
    override def completed() {}
}
