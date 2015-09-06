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

            class OrderPlaced(val price : SignedTicks, val volume : Quantity)
            {
                val events = new Listener(s"$price.$volume")
                val canceller = q store (price, volume, events)
            }

            val _1 = new OrderPlaced(initialPrice, 9)

            checkResult(q, LevelInfo(_1.price, _1.volume :: Nil))
        }

        s"PriceLevel($side)" should "be constructed properly with one order" in new Initial {}

        it should "allow cancel small part of order" in new Initial {
            _1.events.onCancelled expects 5 once ()
            _1.canceller(5)
            checkResult(q, LevelInfo(initialPrice, _1.volume - 5 :: Nil))
        }

        it should "allow cancel order completely" in new Initial {
            _1.events.onCancelled expects _1.volume once ()
            _1.events.onCompleted expects() once()
            _1.canceller(_1.volume)
            checkResult(q, LevelInfo(initialPrice, 0 :: Nil))
        }

        it should "allow cancel more than unmatched amount of order" in new Initial {
            _1.events.onCancelled expects _1.volume once ()
            _1.events.onCompleted expects() once()
            _1.canceller(_1.volume + 5)
            checkResult(q, LevelInfo(initialPrice, 0 :: Nil))
        }

        it should "accept orders of the same price" in new Initial {

            val _2 = new OrderPlaced(initialPrice, 8)

            checkResult(q, LevelInfo(initialPrice, _1.volume :: _2.volume :: Nil))
        }

        it should "match with orders having small price" in new Initial {

            val Incoming = new Listener("Incoming")
            val c1 = 5
            assert(c1 < _1.volume)

            _1.events.onTraded expects (initialPrice, c1) once ()
            Incoming.onTraded expects (initialPrice, c1) once ()

            assert(q.matchWith(initialPrice, c1, Incoming) == 0)
            checkResult(q, LevelInfo(initialPrice, _1.volume - c1 :: Nil))
        }

        it should "ignore orders with too small price" in new Initial {

            val Incoming = new Listener("Incoming")
            val c1 = 5
            assert(c1 < _1.volume)

            val incomingPrice = initialPrice - 1

            assert(q.matchWith(incomingPrice, c1, Incoming) == c1)
            checkResult(q, LevelInfo(initialPrice, _1.volume :: Nil))
        }

        class WithLessAggressive extends Initial {

            val lessAggressivePrice = initialPrice + 5

            val _2 = new OrderPlaced(lessAggressivePrice, 8)

            checkResult(q, LevelInfo(initialPrice, _1.volume :: Nil), LevelInfo(lessAggressivePrice, _2.volume :: Nil))
        }

        class WithTwoLessAggressive extends WithLessAggressive {
            val slightlyLessAggressivePrice = initialPrice + 1

            val _3 = new OrderPlaced(slightlyLessAggressivePrice, 7)

            checkResult(q,
                LevelInfo(initialPrice, _1.volume :: Nil),
                LevelInfo(slightlyLessAggressivePrice, _3.volume :: Nil),
                LevelInfo(lessAggressivePrice, _2.volume :: Nil))
        }

        it should "accept orders of less aggressive price when there are following levels" in new WithTwoLessAggressive {}

    }

}
