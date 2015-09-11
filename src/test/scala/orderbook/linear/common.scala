package orderbook.linear

import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec

object common {

    class Base extends FlatSpec with MockFactory
    {
        class Listener(name : String) extends OrderListener
        {
            val onTraded = mockFunction[Ticks, Quantity, Unit](name + ".onTraded")
            val onCancelled = mockFunction[Quantity, Unit](name + ".onCancelled")
            val onCompleted = mockFunction[Unit](name + ".onCompleted")

            override def traded(price : Ticks, amountTraded : Quantity) = onTraded(price, amountTraded)
            override def cancelled(amount : Quantity) = onCancelled(amount)
            override def completed() = onCompleted()
        }

        val emptyListener = new OrderListener {}

    }

    case class LevelInfo(price : SignedTicks, volumes : List[Quantity])

    def checkResultImpl[Currency](side : Side)(mostAggressive : Option[PriceLevel[Currency]], expected : List[LevelInfo]) : Unit =
    {
        expected match {
            case Nil =>
                assert(mostAggressive.isEmpty || mostAggressive.get.price == TerminalOrderPrice)
            case LevelInfo(price, volumes) :: tail =>
                assert(mostAggressive.nonEmpty)
                val level = mostAggressive.get
                assert(side == level.side)
                assert(price == level.price)
                val volumeTotal = volumes.sum
                assert(volumeTotal == level.totalVolume)
                val actual = level.ownOrders
                actual forall { o => o.side == side && o.price == price }
                val actualVolumes = (actual map { _.unmatchedVolume }).toList
                assert(volumes == actualVolumes)
                level.getNext foreach { N => assert(level == N.getPrevious.get) }

                checkResultImpl(side)(level.getNext, tail)
        }
    }

}
