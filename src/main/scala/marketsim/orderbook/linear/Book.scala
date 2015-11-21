package marketsim
package orderbook
package linear

class Book[Currency](val tickMapper: TickMapper[Currency]) extends AbstractOrderBook[Currency] {

    val Asks = new Queue(Sell)
    val Bids = new Queue(Buy)

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
            val price = order.price
            val q = queue(order.side.opposite)
            q matchWith (price.opposite, order.volume, order) match {
                case 0 =>
                    order fire Completed()
                case unmatched =>
                    val p = queue(order.side)
                    p store  (order, unmatched)
            }
        }

    def process(order : MarketOrder) =
        nonReenterable {
            val q = queue(order.side.opposite)
            q matchWith (MarketOrderPrice, order.volume, order) match {
                case 0 =>
                case unmatched =>
                    order fire Cancelled(order.side, unmatched)
            }
            order fire Completed()
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
