package exchange.simple

import exchange.simple.common._
import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec

class OrderQueueSpec extends FlatSpec with MockFactory {

    Side.choices foreach { side =>

        class Listener() extends OrderListener
        {
            val onCancelled = mockFunction[Quantity, Unit]("onCancelled")
            val onCompleted = mockFunction[Unit]("onCompleted")

            override def cancelled(amount : Quantity) = onCancelled(amount)
            override def completed() = onCompleted()
        }

        val emptyListener = new OrderListener {}

        class Initial {

            val initialPrice = side makeSigned 100

            val queue = new OrderQueue(side)

            def checkResult(expected: LevelInfo*) =
                checkResultImpl(side)(Some(queue.bestLevel), expected.toList)

            val v1 = 9
            val v2 = 8

            val events1 = new Listener
            val cancellation1 = queue store (LimitOrder(side, initialPrice, v1), events1)

            checkResult(LevelInfo(initialPrice, v1 :: Nil))
        }

        s"OrderQueue($side)" should "be constructed properly with one order" in new Initial {}

        it should "allow cancel small part of order" in new Initial {
            events1.onCancelled expects 5 once ()
            cancellation1(5)
            checkResult(LevelInfo(initialPrice, v1 - 5 :: Nil))
        }

        it should "allow cancel order completely" in new Initial {
            events1.onCancelled expects v1 once ()
            events1.onCompleted expects() once()
            cancellation1(v1)
            checkResult(LevelInfo(initialPrice, 0 :: Nil))
        }

        it should "allow cancel more than unmatched amount of order" in new Initial {
            events1.onCancelled expects v1 once ()
            events1.onCompleted expects() once()
            cancellation1(v1 + 5)
            checkResult(LevelInfo(initialPrice, 0 :: Nil))
        }

        it should "accept orders of the same price" in new Initial {

            queue store (LimitOrder(side, initialPrice, v2), emptyListener)

            checkResult(LevelInfo(initialPrice, v1 :: v2 :: Nil))
        }

        class WithMoreAggressive extends Initial {
            val moreAggressivePrice = initialPrice - 3

            queue store (LimitOrder(side, moreAggressivePrice, v2), emptyListener)

            checkResult(LevelInfo(moreAggressivePrice, v2 :: Nil), LevelInfo(initialPrice, v1 :: Nil))

        }

        it should "accept orders of more aggressive price" in new WithMoreAggressive {}

        class WithLessAggressive extends Initial {

            val lessAggressivePrice = initialPrice + 5

            queue store (LimitOrder(side, lessAggressivePrice, v2), emptyListener)

            checkResult(LevelInfo(initialPrice, v1 :: Nil), LevelInfo(lessAggressivePrice, v2 :: Nil))
        }

        it should "accept orders of less aggressive price" in new WithLessAggressive {}

        class WithTwoLessAggressive extends WithLessAggressive {
            val slightlyLessAggressivePrice = initialPrice + 1

            val v3 = 7

            queue store (LimitOrder(side, slightlyLessAggressivePrice, v3), emptyListener)

            checkResult(
                LevelInfo(initialPrice, v1 :: Nil),
                LevelInfo(slightlyLessAggressivePrice, v3 :: Nil),
                LevelInfo(lessAggressivePrice, v2 :: Nil))
        }

        it should "accept orders of less aggressive price when there are following levels" in new WithTwoLessAggressive {}

    }

}
