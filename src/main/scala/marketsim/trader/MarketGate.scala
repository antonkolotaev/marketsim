package marketsim
package trader

import marketsim.reactive.Event

class MarketGate[Currency](book : orderbook.AbstractOrderBook[Currency])
{
    val onLimitOrderSent = new Event[orderbook.LimitOrder]
    val onMarketOrderSent = new Event[orderbook.MarketOrder]
    val onOrderCancelling = new Event[(orderbook.AbstractCanceller, Quantity)]




}
