package marketsim
package orderbook
package linear

class PriceLevelSpec extends common.Base {

    import common._

    Side.choices foreach { side =>

        def checkResult[Currency](mostAggressive : PriceLevel[Currency], expected : LevelInfo*) =
            checkResultImpl(side)(Some(mostAggressive), expected.toList)

        class Initial {

            val initialPrice = Ticks(100) signed side
            val dummy = USD(0)

            val bestPriceLevel = new PriceLevel(TerminalOrderPrice, dummy, None, None)
            val q = new PriceLevel(initialPrice, dummy, None, Some(bestPriceLevel))

            assert(q.totalVolume == 0)
            assert(q.price == initialPrice)
            assert(q.side == side)
            assert(q.allOrders.isEmpty)

            class OrderPlaced(val price : SignedTicks, val volume : Quantity)
            {
                val events = new Listener(s"$price.$volume")
                val canceller = new Canceller
                val order = new LimitOrder(price, volume, events, Some(canceller))

                q store (order, price, dummy, volume, events, Some(canceller))

                def Traded(v : Quantity, incoming : Listener) = { events Traded (price, v, incoming); this }
                def Cancelled (c : Quantity) = { events Cancelled c; this }
                def Completed() = { events Completed(); this }
            }

            val _1 = new OrderPlaced(initialPrice, 9)

            checkResult(q, LevelInfo(_1.price, _1.volume :: Nil))
        }

        s"PriceLevel($side)" should "be constructed properly with one order" in new Initial {}

        it should "allow cancel small part of order" in new Initial {
            _1 Cancelled 5
            _1.canceller(5)
            checkResult(q, LevelInfo(initialPrice, _1.volume - 5 :: Nil))
        }

        it should "allow cancel order completely" in new Initial {
            _1 Cancelled _1.volume Completed()
            _1.canceller(_1.volume)
            checkResult(q, LevelInfo(initialPrice, 0 :: Nil))
        }

        it should "allow cancel more than unmatched amount of order" in new Initial {
            _1 Cancelled _1.volume Completed()
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

            _1 Traded (c1, Incoming)

            assert(q.matchWith(initialPrice, c1, Incoming) == 0)
            checkResult(q, LevelInfo(initialPrice, _1.volume - c1 :: Nil))
        }

        it should "ignore orders with too small price" in new Initial {

            val Incoming = new Listener("Incoming")
            val c1 = 5
            assert(c1 < _1.volume)

            val incomingPrice = initialPrice moreAggressiveBy 1

            assert(q.matchWith(incomingPrice, c1, Incoming) == c1)
            checkResult(q, LevelInfo(initialPrice, _1.volume :: Nil))
        }

        class WithLessAggressive extends Initial {

            val lessAggressivePrice = initialPrice lessAggressiveBy 5

            val _2 = new OrderPlaced(lessAggressivePrice, 8)

            checkResult(q, LevelInfo(initialPrice, _1.volume :: Nil), LevelInfo(lessAggressivePrice, _2.volume :: Nil))
        }

        class WithTwoLessAggressive extends WithLessAggressive {
            val slightlyLessAggressivePrice = initialPrice lessAggressiveBy 1

            val _3 = new OrderPlaced(slightlyLessAggressivePrice, 7)

            checkResult(q,
                LevelInfo(initialPrice, _1.volume :: Nil),
                LevelInfo(slightlyLessAggressivePrice, _3.volume :: Nil),
                LevelInfo(lessAggressivePrice, _2.volume :: Nil))
        }

        it should "accept orders of less aggressive price when there are following levels" in new WithTwoLessAggressive {}

    }

}
