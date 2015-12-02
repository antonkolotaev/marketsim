package marketsim
package trader

/**
 * Number of assets owned by the trader
 * @param trader
 */
class Inventory(trader : MarketClient)
    extends reactive.Variable(0, s"$trader.Position")
{
    trader.onTraded += { trade =>
        set(value +
            (trade.side match {
                    case Buy => +trade.volume
                    case Sell => -trade.volume
            }))
    }
}
