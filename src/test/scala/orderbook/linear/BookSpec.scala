package orderbook.linear

import orderbook.linear.common._

class BookSpec extends Base {

    Side.choices foreach { side =>

        class Initial {

            val initialPrice = 100

            val book = new Book()

            val queue = book queue side
            val queueOpposite = book queue side.opposite

            def checkResult(expected: LevelInfo*)(expectedOpposite : LevelInfo*) = {
                checkResultImpl(side)(Some(queue.bestLevel), expected.toList)
                checkResultImpl(side.opposite)(Some(queueOpposite.bestLevel), expectedOpposite.toList)
            }

            class OrderPlaced(val price : Ticks, val volume : Quantity)
            {
                val signedPrice = side makeSigned price
                val events = new Listener(s"$price.$volume")
                val canceller = new Canceller
                book process LimitOrder(side, price, volume, events, Some(canceller))
            }

            val _1 = new OrderPlaced(initialPrice, 9)

            checkResult(LevelInfo(_1.signedPrice, _1.volume :: Nil))()
        }

        s"OrderBook($side)" should s"be constructed properly with one $side order" in new Initial {}

        it should "allow cancel small part of order" in new Initial {
            _1.events.onCancelled expects 5 once ()
            _1.canceller(5)
            checkResult(LevelInfo(_1.signedPrice, _1.volume - 5 :: Nil))()
        }

        it should "allow cancel order completely" in new Initial {
            _1.events.onCancelled expects _1.volume once ()
            _1.events.onCompleted expects() once()
            _1.canceller(_1.volume)
            checkResult()()
        }

        it should "allow cancel more than unmatched amount of order" in new Initial {
            _1.events.onCancelled expects _1.volume once ()
            _1.events.onCompleted expects() once()
            _1.canceller(_1.volume + 5)
            checkResult()()
        }

        it should "accept orders of the same price" in new Initial {

            val _2 = new OrderPlaced(initialPrice, 8)

            checkResult(LevelInfo(_1.signedPrice, _1.volume :: _2.volume :: Nil))()
        }

        it should "match with orders having small price" in new Initial {

            val Incoming = new Listener("Incoming")
            val c1 = 5
            assert(c1 < _1.volume)

            _1.events.onTraded expects (_1.price, c1) once ()
            Incoming.onTraded expects (_1.price, c1) once ()
            Incoming.onCompleted expects () once ()

            book process LimitOrder(side.opposite, initialPrice, c1, Incoming)

            checkResult(LevelInfo(_1.signedPrice, _1.volume - c1 :: Nil))()
        }

    }

}
