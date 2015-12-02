package marketsim
package trader

class Balance(trader : MarketClient)
    extends reactive.Variable(zeroUSD, trader + ".Balance")
{
    trader.onTraded += { trade =>
        val ticks = trade.price.ticks
        val price = trader.orderbook.tickMapper toCurrency ticks
        val side = trade.side
        val sign = side match { case Buy => -1 case Sell => +1 }
        val delta = price * trade.volume * sign
        set(value_ + delta)
    }
}
