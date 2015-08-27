package exchange

package object simple {

    type Ticks = Int
    type SignedTicks = Int
    type Quantity = Int

    trait Side
    {
        /**
         * aggressive(priceA) > aggressive(priceB) => signed(priceA) < signed(priceB)
         * Sell side: greater price is less aggressive => keep it
         * Buy side: greater price is more aggressive => negate it
         *
         * To get more aggressive signed price one need to subtract positive delta from it
         */
        def makeSigned(price : Ticks) : SignedTicks
    }
    
    object Side {
        def of(price : Ticks) = if (price < 0) Buy else Sell
        def choices = Sell :: Buy :: Nil
    }

    case object Sell extends Side
    {
        def makeSigned(price : Ticks) = price
    }

    case object Buy extends Side
    {
        def makeSigned(price : Ticks) = -price
    }

    trait Order
    {
        val side    : Side
        val volume  : Quantity
    }

    /**
     *  Interface for order event listeners
     */
    trait OrderListener {
        def traded(price : Ticks, amount : Quantity) {}
        def cancelled(amount : Quantity) {}
        def completed() {}
    }


    case class MarketOrder(side : Side, volume : Quantity) extends Order

    case class LimitOrder(side : Side, price : SignedTicks, volume : Quantity) extends Order

    case class LimitOrderInfo(side : Side, price : SignedTicks, unmatchedVolume : Quantity, sender : OrderListener)

    class Entry(private var unmatched : Quantity,
                        val sender : OrderListener)
    {
        def unmatchedVolume = unmatched

        def createInfo(side : Side, price : SignedTicks) =
            LimitOrderInfo(side, price, unmatched, sender)

        def cancel(amount : Quantity) =
        {
            val toCancel = amount min unmatchedVolume
            unmatched -= toCancel
            sender cancelled toCancel
            if (unmatched == 0)
                sender completed ()
            toCancel
        }
    }

    case class CancellationToken(priceLevel: PriceLevel, entry: Entry)
    {
        def apply(amountToCancel : Quantity) = priceLevel cancel (entry, amountToCancel)
    }

    class PriceLevel(val price : SignedTicks,
                     private var prev : Option[PriceLevel],
                     private var next : Option[PriceLevel])
    {
        prev foreach { _.next = Some(this) }
        next foreach { _.prev = Some(this) }

        private val entries = collection.mutable.Queue.empty[Entry]
        private var totalVolume_ : Quantity = 0

        def getPrevious = prev
        def getNext = next

        private def storeImpl(volume : Quantity, sender : OrderListener) =
        {
            val e = new Entry(volume, sender)
            entries enqueue e
            totalVolume_ += volume
            CancellationToken(this, e)
        }

        def store(order : LimitOrder, sender : OrderListener) : CancellationToken =

            if (order.price > price && next.isDefined)
                next.get store (order, sender)
            else
                (if (order.price  < price) new PriceLevel(order.price, prev, Some(this)) else
                 if (order.price == price) this else
                                           new PriceLevel(order.price, Some(this), None)
                    ) storeImpl (order.volume, sender)

        def cancel(e : Entry, amountToCancel : Quantity) = {
            totalVolume_ -= e cancel amountToCancel
        }

        val side = Side of price

        def totalVolume = totalVolume_

        def ownOrders = entries map { _ createInfo (side, price) }
        
        def allOrders : Iterable[LimitOrderInfo] = ownOrders ++ (next map { _.allOrders } getOrElse Nil)
    }

    class OrderQueue(side : Side)
    {
        private var bestPriceLevel = new PriceLevel(Int.MaxValue, None, None)

        def store(order : LimitOrder, sender : OrderListener) = {
            if (order.price < bestPriceLevel.price)
                bestPriceLevel = new PriceLevel(order.price, None, Some(bestPriceLevel))
            bestPriceLevel store(order, sender)
        }

        def bestLevel = bestPriceLevel

        def allOrders = bestPriceLevel.allOrders
    }
}
