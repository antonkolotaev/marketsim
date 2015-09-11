package orderbook.linear

class Book {

    val Asks = new Queue(Sell)
    val Bids = new Queue(Buy)

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

    def process(order : LimitOrder) =
        nonReenterable {
            val price           = order.price signed order.side
            queue(order.side.opposite) matchWith (price.opposite, order.volume, order.sender) match {
                case 0 =>
                    order.sender.completed()
                case unmatched =>
                    queue(order.side) store (price, unmatched, order.sender, order.cancellationKey)
            }
        }

    def process(order : MarketOrder) =
        nonReenterable {
            queue(order.side.opposite) matchWith (MarketOrderPrice, order.volume, order.sender) match {
                case 0 =>
                case unmatched =>
                    order.sender cancelled unmatched
            }
            order.sender.completed()
        }

    def cancel(token : Canceller, amountToCancel : Quantity) = {
        token.side map { queue } foreach { _ cancel (token, amountToCancel) }
    }

}
