package orderbook.linear

class PriceLevelSpec extends common.Base {

    import common._

    Side.choices foreach { side =>

        def checkResult(mostAggressive : PriceLevel, expected : LevelInfo*) =
            checkResultImpl(side)(Some(mostAggressive), expected.toList)

        class Initial {

            val initialPrice = side makeSigned 100

            val bestPriceLevel = new PriceLevel(Int.MaxValue, None, None)
            val q = new PriceLevel(initialPrice, None, Some(bestPriceLevel))

            assert(q.totalVolume == 0)
            assert(q.price == initialPrice)
            assert(q.side == side)
            assert(q.allOrders.isEmpty)

            val v1 = 9
            val v2 = 8

            val events1 = new Listener("1")
            val cancellation1 = q store (initialPrice, v1, events1)

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

            q store (initialPrice, v2, emptyListener)

            checkResult(q, LevelInfo(initialPrice, v1 :: v2 :: Nil))
        }

        it should "match with orders having small price" in new Initial {

            val Incoming = new Listener("Incoming")
            val c1 = 5
            assert(c1 < v1)

            events1.onTraded expects (initialPrice, c1) once ()
            Incoming.onTraded expects (initialPrice, c1) once ()

            assert(q.matchWith(initialPrice, c1, Incoming) == 0)
            checkResult(q, LevelInfo(initialPrice, v1 - c1 :: Nil))
        }

        it should "ignore orders with too small price" in new Initial {

            val Incoming = new Listener("Incoming")
            val c1 = 5
            assert(c1 < v1)

            val incomingPrice = initialPrice - 1

            assert(q.matchWith(incomingPrice, c1, Incoming) == c1)
            checkResult(q, LevelInfo(initialPrice, v1 :: Nil))
        }

        class WithLessAggressive extends Initial {

            val lessAggressivePrice = initialPrice + 5

            q store (lessAggressivePrice, v2, emptyListener)

            checkResult(q, LevelInfo(initialPrice, v1 :: Nil), LevelInfo(lessAggressivePrice, v2 :: Nil))
        }

        class WithTwoLessAggressive extends WithLessAggressive {
            val slightlyLessAggressivePrice = initialPrice + 1

            val v3 = 7

            q store (slightlyLessAggressivePrice, v3, emptyListener)

            checkResult(q,
                LevelInfo(initialPrice, v1 :: Nil),
                LevelInfo(slightlyLessAggressivePrice, v3 :: Nil),
                LevelInfo(lessAggressivePrice, v2 :: Nil))
        }

        it should "accept orders of less aggressive price when there are following levels" in new WithTwoLessAggressive {}

    }

}
