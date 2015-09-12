package orderbook.linear

import orderbook.linear.common._
import reactive.Unary

class BookSpec extends Base {

    Side.choices foreach { side =>

        class Initial {

            val tickMapper = new LinearMapper(USD(10))
            val initialPrice = Ticks(100)
            val initialPriceCurrency = tickMapper toCurrency initialPrice

            val book = new Book(tickMapper)
            book fetchPriceLevelsTillVolume (this, 10)

            val queue = book queue side
            val queueOpposite = book queue side.opposite

            case class QueueState(best : Option[(Ticks, Quantity)],
                                  last : Option[(Ticks, Quantity)],
                                  lasts : List[(Ticks, Quantity)],
                                  levels : List[(USD, Quantity)])

            def toQueueState(queue : Queue[USD]) =
                Unary(ops.and(
                    ops.and(queue.bestPrice, queue.bestPriceVolume),
                    ops.and(
                        ops.and(queue.lastTrade, queue.lastTrades),
                        queue.priceLevels)))
                {
                    case ((Some(p), Some(v)), last) => QueueState(Some(p,v), last._1._1, last._1._2, last._2)
                    case ((None, None), last) => QueueState(None, last._1._1, last._1._2, last._2)
                    case _ => throw new Exception("cannot happen")
                }

            val onChanged =
                mockFunction[(QueueState, QueueState), Unit]("onChanged")

            ops.and(toQueueState(queue), toQueueState(queueOpposite)) += onChanged

            def checkResult(expected: LevelInfo*)(expectedOpposite : LevelInfo*) = {
                checkResultImpl(side)(Some(queue.bestLevel), expected.toList)
                checkResultImpl(side.opposite)(Some(queueOpposite.bestLevel), expectedOpposite.toList)
            }

            class OrderPlaced(val price : Ticks, val volume : Quantity)
            {
                val signedPrice = price signed side
                val events = new Listener(s"$price.$volume")
                val canceller = new Canceller
                book process LimitOrder(side, price, volume, events, Some(canceller))
            }

            onChanged expects (
                QueueState(Some(initialPrice, 9), None, Nil, (initialPriceCurrency, 9) :: Nil),
                QueueState(None, None, Nil, Nil)) once ()

            val _1 = new OrderPlaced(initialPrice, 9)

            checkResult(LevelInfo(_1.signedPrice, _1.volume :: Nil))()
        }

        s"OrderBook($side)" should s"be constructed properly with one $side order" in new Initial {}

        it should "allow cancel small part of order" in new Initial {
            _1.events.onCancelled expects 5 once ()

            onChanged expects (
                QueueState(Some(initialPrice, 9 - 5), None, Nil, (initialPriceCurrency, 9 - 5) :: Nil),
                QueueState(None, None, Nil, Nil)) once ()

            book cancel (_1.canceller, 5)
            checkResult(LevelInfo(_1.signedPrice, _1.volume - 5 :: Nil))()
        }

        it should "allow cancel order completely" in new Initial {
            _1.events.onCancelled expects _1.volume once ()
            _1.events.onCompleted expects() once()
            onChanged expects (QueueState(None, None, Nil, Nil), QueueState(None, None, Nil, Nil)) once ()
            book cancel (_1.canceller, _1.volume)
            checkResult()()
        }

        it should "allow cancel more than unmatched amount of order" in new Initial {
            _1.events.onCancelled expects _1.volume once ()
            _1.events.onCompleted expects() once()
            onChanged expects (QueueState(None, None, Nil, Nil), QueueState(None, None, Nil, Nil)) once ()
            book cancel (_1.canceller, _1.volume + 5)
            checkResult()()
        }

        it should "accept orders of the same price" in new Initial {

            onChanged expects (
                QueueState(Some(initialPrice, 9 + 8), None, Nil, (initialPriceCurrency, (9 + 8) min 10) :: Nil),
                QueueState(None, None, Nil, Nil)) once ()

            val _2 = new OrderPlaced(initialPrice, 8)

            checkResult(LevelInfo(_1.signedPrice, _1.volume :: _2.volume :: Nil))()
        }

        it should "match with limit orders having small price" in new Initial {

            val Incoming = new Listener("Incoming")
            val c1 = 5
            assert(c1 < _1.volume)

            _1.events.onTraded expects (_1.price, c1) once ()
            Incoming.onTraded expects (_1.price, c1) once ()
            Incoming.onCompleted expects () once ()

            onChanged expects (
                QueueState(Some(initialPrice, 9 - 5), Some(initialPrice, 5), (initialPrice, 5) :: Nil, (initialPriceCurrency, 9 - 5) :: Nil),
                QueueState(None, None, Nil, Nil)) once ()

            book process LimitOrder(side.opposite, initialPrice, c1, Incoming)

            checkResult(LevelInfo(_1.signedPrice, _1.volume - c1 :: Nil))()
        }

        it should "match with market orders having small price" in new Initial {

            val Incoming = new Listener("Incoming")
            val c1 = 5
            assert(c1 < _1.volume)

            _1.events.onTraded expects (_1.price, c1) once ()
            Incoming.onTraded expects (_1.price, c1) once ()
            Incoming.onCompleted expects () once ()

            onChanged expects (
                QueueState(Some(initialPrice, 9 - 5), Some(initialPrice, 5), (initialPrice, 5) :: Nil, (initialPriceCurrency, 9 - 5) :: Nil),
                QueueState(None, None, Nil, Nil)) once ()

            book process MarketOrder(side.opposite, c1, Incoming)

            checkResult(LevelInfo(_1.signedPrice, _1.volume - c1 :: Nil))()
        }

        it should "put limit orders with too small price into another queue" in new Initial {

            val Incoming = new Listener("Incoming")
            val c1 = 5
            assert(c1 < _1.volume)

            val incomingPrice = _1.signedPrice moreAggressiveBy 1
            val incomingPriceCurrency = tickMapper toCurrency incomingPrice.ticks

            onChanged expects (
                QueueState(Some(initialPrice, 9), None, Nil, (initialPriceCurrency, 9) :: Nil),
                QueueState(Some(incomingPrice.ticks, 5), None, Nil, (incomingPriceCurrency, 5) :: Nil)) once ()

            book process LimitOrder(side.opposite, incomingPrice.ticks, c1, Incoming)

            checkResult(LevelInfo(_1.signedPrice, _1.volume :: Nil))(LevelInfo(incomingPrice.opposite, c1 :: Nil))
        }

        class WithMoreAggressive extends Initial {
            val moreAggressivePrice = _1.signedPrice moreAggressiveBy 3
            val moreAggressivePriceCurrency = tickMapper toCurrency moreAggressivePrice.ticks

            onChanged expects (
                QueueState(Some(moreAggressivePrice.ticks, 8), None, Nil, (moreAggressivePriceCurrency, 8) :: (initialPriceCurrency, 2) :: Nil),
                QueueState(None, None, Nil, Nil)) once ()

            val _2 = new OrderPlaced(moreAggressivePrice.ticks, 8)

            checkResult(LevelInfo(_2.signedPrice, _2.volume :: Nil), LevelInfo(_1.signedPrice, _1.volume :: Nil))()
        }

        it should "accept orders of more aggressive price" in new WithMoreAggressive {}

        it should "match first order completely with a limit order having too big volume but not very aggressive price" in new WithMoreAggressive {

            val Incoming = new Listener("Incoming")
            val c1 = _1.volume + _2.volume + 5

            val slightlyMoreAggressivePrice = _1.signedPrice moreAggressiveBy 1
            val slightlyMoreAggressivePriceCurrency = tickMapper toCurrency slightlyMoreAggressivePrice.ticks

            _2.events.onTraded expects (_2.price, _2.volume) once ()
            _2.events.onCompleted expects () once ()
            Incoming.onTraded expects (_2.price, _2.volume) once ()

            onChanged expects (
                QueueState(Some(initialPrice, 9),
                    Some(moreAggressivePrice.ticks, _2.volume), (moreAggressivePrice.ticks, _2.volume) :: Nil, (initialPriceCurrency, 9) :: Nil),
                QueueState(Some(slightlyMoreAggressivePrice.ticks, c1 - _2.volume),
                    None, Nil, (slightlyMoreAggressivePriceCurrency, (c1 - _2.volume) min 10) :: Nil)) once ()

            book process LimitOrder(side.opposite, slightlyMoreAggressivePrice.ticks, c1, Incoming)

            checkResult(LevelInfo(_1.signedPrice, _1.volume :: Nil))(LevelInfo(slightlyMoreAggressivePrice.opposite, c1 - _2.volume :: Nil))
        }

        it should "match completely with limit orders having too big volume and not aggressive price" in new WithMoreAggressive {

            val Incoming = new Listener("Incoming")
            val c1 = _1.volume + _2.volume + 5

            val notAggressivePrice = _1.signedPrice lessAggressiveBy 1
            val notAggressivePriceCurrency = tickMapper toCurrency notAggressivePrice.ticks

            _2.events.onTraded expects (_2.price, _2.volume) once ()
            Incoming.onTraded expects (_2.price, _2.volume) once ()
            _2.events.onCompleted expects () once ()

            _1.events.onTraded expects (_1.price, _1.volume) once ()
            Incoming.onTraded expects (_1.price, _1.volume) once ()
            _1.events.onCompleted expects () once ()

            onChanged expects (
                QueueState(None, Some(initialPrice, _1.volume), (_1.price, _1.volume) :: (_2.price, _2.volume) :: Nil, Nil),
                QueueState(Some(notAggressivePrice.ticks, c1 - _2.volume - _1.volume), None, Nil, (notAggressivePriceCurrency, c1 - _2.volume - _1.volume) :: Nil)) once ()

            book process LimitOrder(side.opposite, notAggressivePrice.ticks, c1, Incoming)

            checkResult()(LevelInfo(notAggressivePrice.opposite, c1 - _2.volume - _1.volume :: Nil))
        }

        it should "match completely with market orders having too big volume" in new WithMoreAggressive {

            val Incoming = new Listener("Incoming")
            val c1 = _1.volume + _2.volume + 5

            _2.events.onTraded expects (_2.price, _2.volume) once ()
            Incoming.onTraded expects (_2.price, _2.volume) once ()
            _2.events.onCompleted expects () once ()

            _1.events.onTraded expects (_1.price, _1.volume) once ()
            Incoming.onTraded expects (_1.price, _1.volume) once ()
            _1.events.onCompleted expects () once ()

            Incoming.onCancelled expects c1 - _1.volume - _2.volume once ()
            Incoming.onCompleted expects () once ()

            onChanged expects (
                QueueState(None, Some(initialPrice, _1.volume), (_1.price, _1.volume) :: (_2.price, _2.volume) :: Nil, Nil),
                QueueState(None, None, Nil, Nil)) once ()

            book process MarketOrder(side.opposite, c1, Incoming)

            checkResult()()
        }


    }

}
