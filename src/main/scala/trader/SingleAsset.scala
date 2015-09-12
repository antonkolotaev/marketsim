package trader

import reactive.Variable
import orderbook.linear._

class SingleAsset[Currency](val book : AbstractOrderBook[Currency])(implicit zeroMoney : Currency) extends OrderListener
{
    val position = new Variable(0)
    val balance = new Variable(zeroMoney)

    def sendMarket(price : Currency, volume : Quantity) {}

    override def traded(price : SignedTicks, amount : Quantity) = {
        val priceInCurrency = book.tickMapper toCurrency price.ticks

    }
    override def cancelled(amount : Quantity) {}
    override def completed() {}
}
