package marketsim
package trader

import marketsim.reactive.Variable
import ops._
import orderbook._

object ProfitEstimation
{
    def const[T](x : T) = new Variable(x, x.toString)

    def Rough(trader : MarketClient) =
    {
        val inventory = new Inventory(trader)
        val balance = new Balance(trader)
        val is_positive = inventory > const(0)
        val price = reactive.IfThenElse(is_positive,
                                BestPriceCurrency(trader.orderbook.Bids),
                                BestPriceCurrency(trader.orderbook.Asks))
        //val i2 = inventory + inventory
    }

}