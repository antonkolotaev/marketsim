package orderbook.linear

import orderbook.linear.common._

class QueueSpec extends Base {

    Side.choices foreach { side =>

        class Initial {

            val initialPrice = Ticks(100) signed side

            val queue = new Queue(side)

            def checkResult(expected: LevelInfo*) =
                checkResultImpl(side)(Some(queue.bestLevel), expected.toList)
            
            class OrderPlaced(val price : SignedTicks, val volume : Quantity)
            {
                val events = new Listener(s"$price.$volume")
                val canceller = new Canceller
                queue store (price, volume, events, Some(canceller))
            }

            val _1 = new OrderPlaced(initialPrice, 9)

            checkResult(LevelInfo(initialPrice, _1.volume :: Nil))
        }

        s"OrderQueue($side)" should "be constructed properly with one order" in new Initial {}

        it should "allow cancel small part of order" in new Initial {
            _1.events.onCancelled expects 5 once ()
            queue cancel (_1.canceller, 5)
            checkResult(LevelInfo(initialPrice, _1.volume - 5 :: Nil))
        }

        it should "allow cancel order completely" in new Initial {
            _1.events.onCancelled expects _1.volume once ()
            _1.events.onCompleted expects() once()
            queue cancel (_1.canceller, _1.volume)
            checkResult()
        }

        it should "allow cancel more than unmatched amount of order" in new Initial {
            _1.events.onCancelled expects _1.volume once ()
            _1.events.onCompleted expects() once()
            queue cancel (_1.canceller, _1.volume + 5)
            checkResult()
        }

        it should "accept orders of the same price" in new Initial {

            val _2 = new OrderPlaced(initialPrice, 8)

            checkResult(LevelInfo(initialPrice, _1.volume :: _2.volume :: Nil))
        }

        it should "match with orders having small price" in new Initial {

            val Incoming = new Listener("Incoming")
            val c1 = 5
            assert(c1 < _1.volume)

            _1.events.onTraded expects (initialPrice.ticks, c1) once ()
            Incoming.onTraded expects (initialPrice.ticks, c1) once ()

            assert(queue.matchWith(initialPrice, c1, Incoming) == 0)
            checkResult(LevelInfo(initialPrice, _1.volume - c1 :: Nil))
        }

        it should "ignore orders with too small price" in new Initial {

            val Incoming = new Listener("Incoming")
            val c1 = 5
            assert(c1 < _1.volume)

            val incomingPrice = initialPrice moreAggressiveBy 1

            assert(queue.matchWith(incomingPrice, c1, Incoming) == c1)
            checkResult(LevelInfo(initialPrice, _1.volume :: Nil))
        }

        class WithMoreAggressive extends Initial {
            val moreAggressivePrice = initialPrice moreAggressiveBy 3

            val _2 = new OrderPlaced(moreAggressivePrice, 8)

            checkResult(LevelInfo(moreAggressivePrice, _2.volume :: Nil), LevelInfo(initialPrice, _1.volume :: Nil))
        }

        it should "accept orders of more aggressive price" in new WithMoreAggressive {}

        class WithLessAggressive extends Initial {

            val lessAggressivePrice = initialPrice lessAggressiveBy 5
            
            val _2 = new OrderPlaced(lessAggressivePrice, 8)

            checkResult(LevelInfo(initialPrice, _1.volume :: Nil), LevelInfo(lessAggressivePrice, _2.volume :: Nil))
        }

        it should "accept orders of less aggressive price" in new WithLessAggressive {}

        it should "match completely the first order with an order having the same price" in new WithLessAggressive {

            val Incoming = new Listener("Incoming")

            _1.events.onTraded expects (initialPrice.ticks, _1.volume) once ()
            _1.events.onCompleted expects () once ()
            Incoming.onTraded expects (initialPrice.ticks, _1.volume) once ()

            assert(queue.matchWith(initialPrice, _1.volume, Incoming) == 0)
            checkResult(LevelInfo(lessAggressivePrice, _2.volume :: Nil))
        }

        it should "match completely the first order with a big order having a less aggressive price" in new WithLessAggressive {

            val Incoming = new Listener("Incoming")

            val c = _1.volume + 7

            val p = initialPrice lessAggressiveBy 1
            assert(p isMoreAggressiveThan lessAggressivePrice)

            _1.events.onTraded expects (initialPrice.ticks, _1.volume) once ()
            _1.events.onCompleted expects () once ()
            Incoming.onTraded expects (initialPrice.ticks, _1.volume) once ()

            assert(queue.matchWith(p, c, Incoming) == c - _1.volume)
            checkResult(LevelInfo(lessAggressivePrice, _2.volume :: Nil))
        }

        it should "empty the queue being matched with a very big order" in new WithLessAggressive {

            val Incoming = new Listener("Incoming")

            val c = _1.volume + _2.volume + 5

            _1.events.onTraded expects (initialPrice.ticks, _1.volume) once ()
            _1.events.onCompleted expects () once ()

            _2.events.onTraded expects (lessAggressivePrice.ticks, _2.volume) once ()
            _2.events.onCompleted expects () once ()

            Incoming.onTraded expects (initialPrice.ticks, _1.volume) once ()
            Incoming.onTraded expects (lessAggressivePrice.ticks, _2.volume) once ()

            assert(queue.matchWith(MarketOrderPrice, c, Incoming) == c - _1.volume - _2.volume)
            checkResult()
        }

        class WithTwoLessAggressive extends WithLessAggressive {
            val slightlyLessAggressivePrice = initialPrice lessAggressiveBy 1

            val v3 = 7

            queue store (slightlyLessAggressivePrice, v3, emptyListener, None)

            checkResult(
                LevelInfo(initialPrice, _1.volume :: Nil),
                LevelInfo(slightlyLessAggressivePrice, v3 :: Nil),
                LevelInfo(lessAggressivePrice, _2.volume :: Nil))
        }

        it should "accept orders of less aggressive price when there are following levels" in new WithTwoLessAggressive {}

    }

}
