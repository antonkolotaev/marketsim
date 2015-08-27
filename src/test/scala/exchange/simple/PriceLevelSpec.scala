package exchange.simple

import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec

class PriceLevelSpec extends FlatSpec with MockFactory {

    val emptyListener = new OrderListener {}

    Side.choices foreach { side =>

        case class LevelInfo(price : SignedTicks, volumes : List[Quantity])

        def checkResultImpl(mostAggressive : Option[PriceLevel], expected : List[LevelInfo]) : Unit =
        {
            expected match {
                case Nil =>
                    assert(mostAggressive.isEmpty)
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

                    checkResultImpl(level.getNext, tail)
            }
        }

        def checkResult(mostAggressive : PriceLevel, expected : LevelInfo*) =
            checkResultImpl(Some(mostAggressive), expected.toList)

        def check(queue : OrderQueue, expected : LevelInfo*) =
            checkResultImpl(Some(queue.bestLevel), expected.toList)

        class Listener() extends OrderListener
        {
            val onCancelled = mockFunction[Quantity, Unit]("onCancelled")
            val onCompleted = mockFunction[Unit]("onCompleted")

            override def cancelled(amount : Quantity) = onCancelled(amount)
            override def completed() = onCompleted()
        }

        class Initial {

            val initialPrice = side makeSigned 100

            val q = new PriceLevel(initialPrice, None, None)

            assert(q.totalVolume == 0)
            assert(q.price == initialPrice)
            assert(q.side == side)
            assert(q.allOrders.isEmpty)

            val v1 = 9
            val v2 = 8

            val events1 = new Listener
            val cancellation1 = q store (LimitOrder(side, initialPrice, v1), events1)

            checkResult(q, LevelInfo(initialPrice, v1 :: Nil))
        }

        s"PriceLevel($side)" should "be constructed properly with one order" in new Initial {}

        it should "allow cancel small part of order" in new Initial {
            events1.onCancelled expects 5 once ()
            cancellation1(5)
            checkResult(q, LevelInfo(initialPrice, v1 - 5 :: Nil))
        }

        it should "allow cancel order completely" in new Initial {
            events1.onCancelled expects v1 once ()
            events1.onCompleted expects() once()
            cancellation1(v1)
            checkResult(q, LevelInfo(initialPrice, 0 :: Nil))
        }

        it should "allow cancel more than unmatched amount of order" in new Initial {
            events1.onCancelled expects v1 once ()
            events1.onCompleted expects() once()
            cancellation1(v1 + 5)
            checkResult(q, LevelInfo(initialPrice, 0 :: Nil))
        }

        it should "accept orders of the same price" in new Initial {

            q store (LimitOrder(side, initialPrice, v2), emptyListener)

            checkResult(q, LevelInfo(initialPrice, v1 :: v2 :: Nil))
        }

        class WithMoreAggressive extends Initial {
            val moreAggressivePrice = initialPrice - 3

            q store (LimitOrder(side, moreAggressivePrice, v2), emptyListener)

            val p = q.getPrevious.get

            checkResult(p, LevelInfo(moreAggressivePrice, v2 :: Nil), LevelInfo(initialPrice, v1 :: Nil))

        }

        it should "accept orders of more aggressive price" in new WithMoreAggressive {}

        class WithLessAggressive extends Initial {

            val lessAggressivePrice = initialPrice + 5

            q store (LimitOrder(side, lessAggressivePrice, v2), emptyListener)

            checkResult(q, LevelInfo(initialPrice, v1 :: Nil), LevelInfo(lessAggressivePrice, v2 :: Nil))
        }

        it should "accept orders of less aggressive price" in new WithLessAggressive {}

        class WithTwoLessAggressive extends WithLessAggressive {
            val slightlyLessAggressivePrice = initialPrice + 1

            val v3 = 7

            q store (LimitOrder(side, slightlyLessAggressivePrice, v3), emptyListener)

            checkResult(q,
                LevelInfo(initialPrice, v1 :: Nil),
                LevelInfo(slightlyLessAggressivePrice, v3 :: Nil),
                LevelInfo(lessAggressivePrice, v2 :: Nil))
        }

        it should "accept orders of less aggressive price when there are following levels" in new WithTwoLessAggressive {}
    }

}
