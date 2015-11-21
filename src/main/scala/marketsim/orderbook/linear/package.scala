package marketsim
package orderbook

package object linear {


    val TerminalOrderPrice = SignedTicks(Int.MaxValue)
    val MarketOrderPrice = TerminalOrderPrice moreAggressiveBy 1


    def cancellableLimitOrder(side: Side,
                              price: Ticks,
                              volume: Quantity,
                              sender: OrderListener) =
        LimitOrder(price signed side, volume, sender, Some(new linear.Canceller))

}

