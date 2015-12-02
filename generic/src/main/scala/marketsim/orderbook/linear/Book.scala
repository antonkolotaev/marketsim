package marketsim
package orderbook
package linear

class Book(val tickMapper: TickMapper) extends AbstractOrderBook {

    val infiniteCurrency = tickMapper toCurrency TerminalOrderPrice.ticks
    val Asks = new OrderQueue(Sell, infiniteCurrency)
    val Bids = new OrderQueue(Buy, infiniteCurrency)

    def cancellationToken = new CancellationToken

    def queue(side : Side) = side match {
        case Sell => Asks
        case Buy => Bids
    }

    type CancellationToken = marketsim.orderbook.linear.Canceller
    type OrderQueue = marketsim.orderbook.linear.Queue

    override def toString = s"Asks($Asks)Bids($Bids)"

    private var entered = false

    private def nonReenterable[T](f : => T) = {
        if (entered)
            throw new Exception("reentering into a blocking operation on order book")
        entered = true
        val ret = f
        entered = false
        ret
    }

    def fetchPriceLevelsTillVolume(limitVolume : Quantity) {
        Bids fetchPriceLevelsTillVolume limitVolume
        Asks fetchPriceLevelsTillVolume limitVolume
    }

    def process(order : LimitOrder) =
        nonReenterable {
            val price = order.price signed order.side
            val q = queue(order.side.opposite)
            q matchWith (price.opposite, order.volume, order.sender) match {
                case 0 =>
                    order.sender handle Completed()
                case unmatched =>
                    val p = queue(order.side)
                    val cancellationToken = order.cancellationKey map { x => x.asInstanceOf[CancellationToken] }
                    p store  (price, tickMapper toCurrency order.price, unmatched, order.sender, cancellationToken)
            }
        }

    def process(order : MarketOrder) =
        nonReenterable {
            val q = queue(order.side.opposite)
            q matchWith (MarketOrderPrice, order.volume, order.sender) match {
                case 0 =>
                case unmatched =>
                    order.sender handle Cancelled(order.side, unmatched)
            }
            order.sender handle Completed()
        }

    def cancel(token : CancellationToken, amountToCancel : Quantity) = {
        nonReenterable {
            token.side map { queue } foreach { queue =>
                queue cancel(token, amountToCancel)
            }
        }
    }

}
