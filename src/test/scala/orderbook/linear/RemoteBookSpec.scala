package orderbook.linear

import orderbook.linear.common._
import reactive.Unary

class RemoteBookSpec extends Base {

    Side.choices foreach { side =>

        class Initial {

            val scheduler = core.Scheduler.recreate()

            val tickMapper = new LinearMapper(cents(1))
            val initialPrice = Ticks(100)

            val fetchVolume = 10

            val localBook = new Book(tickMapper)
            localBook fetchPriceLevelsTillVolume(this, fetchVolume)

            val localQueue = localBook queue side
            val localQueueOpposite = localBook queue side.opposite

            val up = core.Duration(3)
            val down = core.Duration(5)
            val up_down = up + down
            val epsilon = core.Duration(1)

            val remoteBook = new Remote.Book(localBook, up, down)
            val remoteQueue = remoteBook queue side
            val remoteQueueOpposite = remoteBook queue side.opposite

            case class QueueState(best: Option[(Ticks, Quantity)],
                                  last: Option[(Ticks, Quantity)],
                                  lasts: List[(Ticks, Quantity)],
                                  levels: List[(USD, Quantity)]) {
                def trades(lasts: (Ticks, Quantity)*) =
                    copy(lasts = lasts.toList, last = lasts.headOption)

                def levels(levels: (Ticks, Quantity)*) =
                    copy(levels = levels.toList map { case (t, v) => (tickMapper toCurrency t, v min fetchVolume) },
                        best = levels.headOption)
            }

            val E = QueueState(None, None, Nil, Nil)

            def toQueueState(queue: AbstractOrderQueue[USD]) =
                Unary(ops.and(
                    ops.and(queue.bestPrice, queue.bestPriceVolume),
                    ops.and(
                        ops.and(queue.lastTrade, queue.lastTrades),
                        queue.priceLevels))) {
                    case ((Some(p), Some(v)), last) => QueueState(Some(p, v), last._1._1, last._1._2, last._2)
                    case ((None, None), last) => QueueState(None, last._1._1, last._1._2, last._2)
                    case _ => throw new Exception("cannot happen")
                }

            val onChangedLocally =
                mockFunction[(QueueState, QueueState, core.Time), Unit]("onChangedLocally")

            ops.and(toQueueState(localQueue), toQueueState(localQueueOpposite)) += {
                case (q,p) => onChangedLocally(q, p, core.Scheduler.currentTime)
            }

            val onChangedRemotely =
                mockFunction[(QueueState, QueueState, core.Time), Unit]("onChangedRemotely")

            ops.and(toQueueState(remoteQueue), toQueueState(remoteQueueOpposite)) += {
                case (q,p) => onChangedRemotely(q, p, core.Scheduler.currentTime)
            }

            def changesExpected(q : QueueState, p : QueueState) = {
                onChangedLocally expects (q,p,after(up)) once ()
                onChangedRemotely expects(q,p, after(up_down)) once ()
            }

            def checkLocalResult(expected: LevelInfo*)(expectedOpposite: LevelInfo*) = {
                checkResultImpl(side)(Some(localQueue.bestLevel), expected.toList)
                checkResultImpl(side.opposite)(Some(localQueueOpposite.bestLevel), expectedOpposite.toList)
            }

            class OrderPlaced(val price: Ticks, val volume: Quantity) {
                val signedPrice = price signed side
                val events = new Listener(s"$price.$volume")
                val canceller = new Canceller
                remoteBook process LimitOrder(side, price, volume, events, Some(canceller))
            }

            def after(dt : core.Duration) = core.Scheduler.currentTime + dt

            val V1 = 9

            changesExpected(E.levels((initialPrice, V1)), E)

            val _1 = new OrderPlaced(initialPrice, V1)

            scheduler advance up_down + epsilon

            checkLocalResult(LevelInfo(_1.signedPrice, _1.volume :: Nil))()
        }

        s"OrderBook($side)" should s"be constructed properly with one $side order" in new Initial {}

        it should "allow cancel small part of order" in new Initial {

            val C1 = 5
            assert(C1 < V1)

            _1.events.onCancelled expects C1 once()

            changesExpected(E.levels((initialPrice, V1 - C1)), E)

            remoteBook cancel(_1.canceller, C1)

            scheduler advance up_down + epsilon

            checkLocalResult(LevelInfo(_1.signedPrice, _1.volume - C1 :: Nil))()
        }

        it should "allow cancel order completely" in new Initial {
            _1.events.onCancelled expects _1.volume once()
            _1.events.onCompleted expects() once()
            changesExpected(E, E)
            remoteBook cancel(_1.canceller, _1.volume)
            scheduler advance up_down + epsilon
            checkLocalResult()()
        }

        it should "allow cancel more than unmatched amount of order" in new Initial {
            _1.events.onCancelled expects _1.volume once()
            _1.events.onCompleted expects() once()
            changesExpected(E, E)
            remoteBook cancel(_1.canceller, _1.volume + 5)
            scheduler advance up_down + epsilon
            checkLocalResult()()
        }

        it should "accept orders of the same price" in new Initial {

            val V2 = 8
            changesExpected(E levels ((initialPrice, V1 + V2)), E)

            val _2 = new OrderPlaced(initialPrice, V2)

            scheduler advance up_down + epsilon

            checkLocalResult(LevelInfo(_1.signedPrice, _1.volume :: _2.volume :: Nil))()
        }

        it should "match with limit orders having small price" in new Initial {

            val Incoming = new Listener("Incoming")
            val c1 = 5
            assert(c1 < _1.volume)

            _1.events.onTraded expects(_1.price, c1) once()
            Incoming.onTraded expects(_1.price, c1) once()
            Incoming.onCompleted expects() once()

            changesExpected(E levels ((initialPrice, V1 - c1)) trades ((initialPrice, c1)), E)

            remoteBook process LimitOrder(side.opposite, initialPrice, c1, Incoming)

            scheduler advance up_down + epsilon

            checkLocalResult(LevelInfo(_1.signedPrice, _1.volume - c1 :: Nil))()
        }

        it should "match with market orders having small price" in new Initial {

            val Incoming = new Listener("Incoming")
            val c1 = 5
            assert(c1 < _1.volume)

            _1.events.onTraded expects(_1.price, c1) once()
            Incoming.onTraded expects(_1.price, c1) once()
            Incoming.onCompleted expects() once()

            changesExpected(E levels ((initialPrice, V1 - c1)) trades ((initialPrice, c1)), E)

            remoteBook process MarketOrder(side.opposite, c1, Incoming)

            scheduler advance up_down + epsilon

            checkLocalResult(LevelInfo(_1.signedPrice, _1.volume - c1 :: Nil))()
        }

        it should "put limit orders with too small price into another queue" in new Initial {

            val Incoming = new Listener("Incoming")
            val c1 = 5
            assert(c1 < _1.volume)

            val incomingPrice = _1.signedPrice moreAggressiveBy 1

            changesExpected(E levels ((initialPrice, V1)), E levels ((incomingPrice.ticks, c1)))

            remoteBook process LimitOrder(side.opposite, incomingPrice.ticks, c1, Incoming)

            scheduler advance up_down + epsilon

            checkLocalResult(LevelInfo(_1.signedPrice, _1.volume :: Nil))(LevelInfo(incomingPrice.opposite, c1 :: Nil))
        }

        class WithMoreAggressive extends Initial {
            val moreAggressivePrice = _1.signedPrice moreAggressiveBy 3

            val V2 = 8
            changesExpected(E levels ((moreAggressivePrice.ticks, V2), (initialPrice, fetchVolume - V2)), E)

            val _2 = new OrderPlaced(moreAggressivePrice.ticks, V2)

            scheduler advance up_down + epsilon

            checkLocalResult(LevelInfo(_2.signedPrice, _2.volume :: Nil), LevelInfo(_1.signedPrice, _1.volume :: Nil))()
        }

        it should "accept orders of more aggressive price" in new WithMoreAggressive {}

        it should "match first order completely with a limit order having too big volume but not very aggressive price" in new WithMoreAggressive {

            val Incoming = new Listener("Incoming")
            val c1 = _1.volume + _2.volume + 5

            val slightlyMoreAggressivePrice = _1.signedPrice moreAggressiveBy 1

            _2.events.onTraded expects(_2.price, _2.volume) once()
            _2.events.onCompleted expects() once()
            Incoming.onTraded expects(_2.price, _2.volume) once()

            changesExpected(
                E levels ((initialPrice, V1)) trades ((moreAggressivePrice.ticks, _2.volume)),
                E levels ((slightlyMoreAggressivePrice.ticks, c1 - _2.volume)))

            remoteBook process LimitOrder(side.opposite, slightlyMoreAggressivePrice.ticks, c1, Incoming)

            scheduler advance up_down + epsilon

            checkLocalResult(LevelInfo(_1.signedPrice, _1.volume :: Nil))(LevelInfo(slightlyMoreAggressivePrice.opposite, c1 - _2.volume :: Nil))
        }

        it should "match completely with limit orders having too big volume and not aggressive price" in new WithMoreAggressive {

            val Incoming = new Listener("Incoming")
            val c1 = _1.volume + _2.volume + 5

            val notAggressivePrice = _1.signedPrice lessAggressiveBy 1

            _2.events.onTraded expects(_2.price, _2.volume) once()
            Incoming.onTraded expects(_2.price, _2.volume) once()
            _2.events.onCompleted expects() once()

            _1.events.onTraded expects(_1.price, _1.volume) once()
            Incoming.onTraded expects(_1.price, _1.volume) once()
            _1.events.onCompleted expects() once()

            changesExpected(
                E trades((_1.price, _1.volume), (_2.price, _2.volume)),
                E levels ((notAggressivePrice.ticks, c1 - _2.volume - _1.volume)))

            remoteBook process LimitOrder(side.opposite, notAggressivePrice.ticks, c1, Incoming)

            scheduler advance up_down + epsilon

            checkLocalResult()(LevelInfo(notAggressivePrice.opposite, c1 - _2.volume - _1.volume :: Nil))
        }

        it should "match completely with market orders having too big volume" in new WithMoreAggressive {

            val Incoming = new Listener("Incoming")
            val c1 = _1.volume + _2.volume + 5

            _2.events.onTraded expects(_2.price, _2.volume) once()
            Incoming.onTraded expects(_2.price, _2.volume) once()
            _2.events.onCompleted expects() once()

            _1.events.onTraded expects(_1.price, _1.volume) once()
            Incoming.onTraded expects(_1.price, _1.volume) once()
            _1.events.onCompleted expects() once()

            Incoming.onCancelled expects c1 - _1.volume - _2.volume once()
            Incoming.onCompleted expects() once()

            changesExpected(E trades((_1.price, _1.volume), (_2.price, _2.volume)), E)

            remoteBook process MarketOrder(side.opposite, c1, Incoming)

            scheduler advance up_down + epsilon

            checkLocalResult()()
        }
    }
}