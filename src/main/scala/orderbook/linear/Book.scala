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

    private val emptyCancellation = (q : Quantity) => q

    def processLimit(order : LimitOrder, sender : OrderListener) : CancellationToken = {
        underLock[CancellationToken] {
            queue(order.side.opposite) matchWith (order.price, order.volume, sender) match {
                case 0 => emptyCancellation
                //case unmatched =>
                //    queue(order.side) store ()
            }

        }
    }

}
