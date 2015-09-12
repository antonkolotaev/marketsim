package orderbook.linear

import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec

object common {

    class Base extends FlatSpec with MockFactory
    {
        class Listener(name : String) extends OrderListener
        {
            val onTraded = mockFunction[SignedTicks, Quantity, Unit](name + ".onTraded")
            val onCancelled = mockFunction[Quantity, Unit](name + ".onCancelled")
            val onCompleted = mockFunction[Unit](name + ".onCompleted")

            override def traded(price : SignedTicks, amountTraded : Quantity) = onTraded(price, amountTraded)
            override def cancelled(amount : Quantity) = onCancelled(amount)
            override def completed() = onCompleted()

            def Cancelled(c : Quantity) = {
                onCancelled expects c once()
                this
            }

            def Completed() = {
                onCompleted expects() once()
                this
            }

            def Traded(p : SignedTicks, v : Quantity, incoming : Listener) = {
                onTraded expects (p, v) once ()
                incoming.onTraded expects (p.opposite, v) once ()
                this
            }

        }

        class ListenerWithTime(name : String, up_down : core.Duration) extends OrderListener
        {
            val onTraded = mockFunction[SignedTicks, Quantity, core.Time, Unit](name + ".onTraded")
            val onCancelled = mockFunction[Quantity, core.Time, Unit](name + ".onCancelled")
            val onCompleted = mockFunction[core.Time, Unit](name + ".onCompleted")

            override def traded(price : SignedTicks, amountTraded : Quantity) = onTraded(price, amountTraded, core.Scheduler.currentTime)
            override def cancelled(amount : Quantity) = onCancelled(amount, core.Scheduler.currentTime)
            override def completed() = onCompleted(core.Scheduler.currentTime)

            def after(dt : core.Duration) = core.Scheduler.currentTime + dt

            def Traded(price : SignedTicks, tradeVolume : Quantity, incomingEvents : ListenerWithTime) = {
                onTraded expects(price, tradeVolume, after(up_down)) once()
                incomingEvents.onTraded expects(price.opposite, tradeVolume, after(up_down)) once()
                this
            }

            def Completed() = {
                onCompleted expects after(up_down) once()
                this
            }

            def Cancelled(amount : Quantity) = {
                onCancelled expects(amount, after(up_down)) once()
                this
            }
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
