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
