package marketsim
package orderbook
package linear

class Book[Currency](val tickMapper: TickMapper[Currency]) extends AbstractOrderBook[Currency] {

    val infiniteCurrency = tickMapper toCurrency TerminalOrderPrice.ticks
    val Asks = new Queue(Sell, infiniteCurrency)
    val Bids = new Queue(Buy, infiniteCurrency)

    def cancellationToken = new Canceller

    def queue(side : Side) = side match {
        case Sell => Asks
        case Buy => Bids
    }

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
                    val cancellationToken = order.cancellationKey map { x => x.asInstanceOf[Canceller] }
                    p store  (order, price, tickMapper toCurrency order.price, unmatched, order.sender, cancellationToken)
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

    def cancel(token : AbstractCanceller, amountToCancel : Quantity) = {
        nonReenterable {
            val t = token.asInstanceOf[Canceller]
            t.side map { queue } foreach { queue =>
                queue cancel(t, amountToCancel)
            }
        }
    }

}
