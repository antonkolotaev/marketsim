package orderbook.linear

import orderbook.linear.common._

class OrderQueueSpec extends Base {

    Side.choices foreach { side =>

        class Initial {

            val initialPrice = side makeSigned 100

            val queue = new OrderQueue(side)

            def checkResult(expected: LevelInfo*) =
                checkResultImpl(side)(Some(queue.bestLevel), expected.toList)

            val v1 = 9
            val v2 = 8

            val events1 = new Listener("1")
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

        it should "match with orders having small price" in new Initial {

            val Incoming = new Listener("Incoming")
            val c1 = 5
            assert(c1 < v1)

            events1.onTraded expects (initialPrice, c1) once ()
            Incoming.onTraded expects (initialPrice, c1) once ()

            assert(queue.matchWith(c1, initialPrice, Incoming) == 0)
            checkResult(LevelInfo(initialPrice, v1 - c1 :: Nil))
        }

        it should "ignore orders with too small price" in new Initial {

            val Incoming = new Listener("Incoming")
            val c1 = 5
            assert(c1 < v1)

            val incomingPrice = initialPrice - 1

            assert(queue.matchWith(c1, incomingPrice, Incoming) == c1)
            checkResult(LevelInfo(initialPrice, v1 :: Nil))
        }

        class WithMoreAggressive extends Initial {
            val moreAggressivePrice = initialPrice - 3

            queue store (LimitOrder(side, moreAggressivePrice, v2), emptyListener)

            checkResult(LevelInfo(moreAggressivePrice, v2 :: Nil), LevelInfo(initialPrice, v1 :: Nil))
        }

        it should "accept orders of more aggressive price" in new WithMoreAggressive {}

        class WithLessAggressive extends Initial {

            val lessAggressivePrice = initialPrice + 5

            val events2 = new Listener("2")

            queue store (LimitOrder(side, lessAggressivePrice, v2), events2)

            checkResult(LevelInfo(initialPrice, v1 :: Nil), LevelInfo(lessAggressivePrice, v2 :: Nil))
        }

        it should "accept orders of less aggressive price" in new WithLessAggressive {}

        it should "match completely the first order with an order having the same price" in new WithLessAggressive {

            val Incoming = new Listener("Incoming")

            events1.onTraded expects (initialPrice, v1) once ()
            events1.onCompleted expects () once ()
            Incoming.onTraded expects (initialPrice, v1) once ()

            assert(queue.matchWith(v1, initialPrice, Incoming) == 0)
            checkResult(LevelInfo(lessAggressivePrice, v2 :: Nil))
        }

        it should "match completely the first order with a big order having a less aggressive price" in new WithLessAggressive {

            val Incoming = new Listener("Incoming")

            val c = v1 + 7

            val p = initialPrice + 1
            assert(p < lessAggressivePrice)

            events1.onTraded expects (initialPrice, v1) once ()
            events1.onCompleted expects () once ()
            Incoming.onTraded expects (initialPrice, v1) once ()

            assert(queue.matchWith(c, p, Incoming) == c - v1)
            checkResult(LevelInfo(lessAggressivePrice, v2 :: Nil))
        }

        it should "empty the queue being matched with a very big order" in new WithLessAggressive {

            val Incoming = new Listener("Incoming")

            val c = v1 + v2 + 5

            events1.onTraded expects (initialPrice, v1) once ()
            events1.onCompleted expects () once ()

            events2.onTraded expects (lessAggressivePrice, v2) once ()
            events2.onCompleted expects () once ()

            Incoming.onTraded expects (initialPrice, v1) once ()
            Incoming.onTraded expects (lessAggressivePrice, v2) once ()

            assert(queue.matchWith(c, MarketOrderPrice, Incoming) == c - v1 - v2)
            checkResult()
        }

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
