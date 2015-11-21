package marketsim {
package orderbook {
package linear {

    import org.scalamock.scalatest.MockFactory
    import org.scalatest.FlatSpec

    object common {

        class Base extends FlatSpec with MockFactory {

            class Listener(name: String) extends OrderListener {
                val onTraded = mockFunction[Traded, Unit](name + ".onTraded")
                val onCancelled = mockFunction[Quantity, Unit](name + ".onCancelled")
                val onCompleted = mockFunction[Unit](name + ".onCompleted")

                override def handle(traded: Traded) = onTraded(traded)

                override def handle(cancelled: Cancelled) = onCancelled(cancelled.amount)

                override def handle(completed: Completed) = onCompleted()

                def Cancelled(c: Quantity) = {
                    onCancelled expects c once()
                    this
                }

                def Completed() = {
                    onCompleted expects() once()
                    this
                }

                def Traded(p: SignedTicks, v: Quantity, incoming: Listener) = {
                    onTraded expects orderbook.linear.Traded(p, v) once()
                    incoming.onTraded expects orderbook.linear.Traded(p.opposite, v) once()
                    this
                }

            }

            class ListenerWithTime(name: String, up_down: marketsim.core.Duration) extends OrderListener {
                val onTraded = mockFunction[Traded, marketsim.core.Time, Unit](name + ".onTraded")
                val onCancelled = mockFunction[Quantity, marketsim.core.Time, Unit](name + ".onCancelled")
                val onCompleted = mockFunction[marketsim.core.Time, Unit](name + ".onCompleted")

                override def handle(traded: Traded) = onTraded(traded, core.Scheduler.currentTime)

                override def handle(cancelled: Cancelled) = onCancelled(cancelled.amount, core.Scheduler.currentTime)

                override def handle(completed: Completed) = onCompleted(core.Scheduler.currentTime)

                def after(dt: marketsim.core.Duration) = core.Scheduler.currentTime + dt

                def Traded(price: SignedTicks, tradeVolume: Quantity, incomingEvents: ListenerWithTime) = {
                    onTraded expects(orderbook.linear.Traded(price, tradeVolume), after(up_down)) once()
                    incomingEvents.onTraded expects(orderbook.linear.Traded(price.opposite, tradeVolume), after(up_down)) once()
                    this
                }

                def Completed() = {
                    onCompleted expects after(up_down) once()
                    this
                }

                def Cancelled(amount: Quantity) = {
                    onCancelled expects(amount, after(up_down)) once()
                    this
                }
            }

            val emptyListener = new OrderListener {}

        }

        case class LevelInfo(price: SignedTicks, volumes: List[Quantity])

        def checkResultImpl[Currency](side: Side)(mostAggressive: Option[PriceLevel[Currency]], expected: List[LevelInfo]): Unit = {
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
                    val actualVolumes = (actual map {
                        _.unmatchedVolume
                    }).toList
                    assert(volumes == actualVolumes)
                    level.getNext foreach { N => assert(level == N.getPrevious.get) }

                    checkResultImpl(side)(level.getNext, tail)
            }
        }

    }

}}}