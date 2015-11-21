package marketsim
package orderbook
package linear

class Canceller extends AbstractCanceller {
    private var entry_level = Option.empty[(Entry, SamePriceOrders)]

    private[linear] def set(e: Entry, level: SamePriceOrders) = {
        entry_level = Some((e, level))
    }

    def side = entry_level map {
        _._2.price.side
    }

    private[linear] def apply(amountToCancel: Quantity) = {
        entry_level match {
            case Some((entry, level)) => level cancel(entry, amountToCancel)
            case None =>
            //throw new Exception("CancellationKeyImpl is not initialized")
        }
    }
}
