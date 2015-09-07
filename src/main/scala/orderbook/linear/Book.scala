package orderbook.linear

class Book {

    val Asks = new Queue(Sell)
    val Bids = new Queue(Buy)

    def queue(side : Side) = side match {
        case Sell => Asks
        case Buy => Bids
    }

    private var locked = false

    private def underLock[T](f : => T) = {
        if (locked)
            throw new Exception("reentering into a blocking operation on order book")
        locked = true
        val ret = f
        locked = false
        ret
    }

    def process(order : LimitOrder) =
        underLock {
            val price = order.side makeSigned order.price
            val opposite_price = order.side.opposite makeSigned order.price
            queue(order.side.opposite) matchWith (opposite_price, order.volume, order.sender) match {
                case 0 =>
                    order.sender.completed()
                case unmatched =>
                    queue(order.side) store (price, unmatched, order.sender, order.cancellationKey)
            }
        }

    def process(order : MarketOrder) =
        underLock {
            queue(order.side.opposite) matchWith (MarketOrderPrice, order.volume, order.sender) match {
                case 0 =>
                case unmatched =>
                    order.sender cancelled unmatched
            }
            order.sender.completed()
        }

    def cancel(token : Canceller, amountToCancel : Quantity) = token(amountToCancel)

}
