package marketsim
package orderbook

package object linear {


    val TerminalOrderPrice = SignedTicks(Int.MaxValue)
    val MarketOrderPrice = TerminalOrderPrice moreAggressiveBy 1


}

