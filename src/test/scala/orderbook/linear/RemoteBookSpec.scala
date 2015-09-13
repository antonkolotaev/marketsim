package orderbook.linear

import orderbook.linear.common._
import reactive.Unary

class RemoteBookSpec extends Base {

    Side.choices foreach { side =>

        class Initial {

            val scheduler = core.Scheduler.recreate()
            Remote.recreateDelayedListeners()

            val tickMapper = new LinearMapper(cents(1))
            val initialPrice = Ticks(100)

            val fetchVolume = 10

            val localBook = new Book(tickMapper)
            val localQueue = localBook queue side
            val localQueueOpposite = localBook queue side.opposite

            val up = core.Duration(3)
            val down = core.Duration(5)
            val up_down = up + down
            val epsilon = core.Duration(1)

            val remoteBook = new Remote.Book(localBook, up, down)
            val remoteQueue = remoteBook queue side
            val remoteQueueOpposite = remoteBook queue side.opposite

            remoteBook fetchPriceLevelsTillVolume  fetchVolume

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

            import ops._

            def toQueueState(queue: AbstractOrderQueue[USD]) =
                Unary((queue.bestPrice and queue.bestPriceVolume) and ((queue.lastTrade and queue.lastTrades) and queue.priceLevels)) {
                    case ((Some(p), Some(v)), last) => QueueState(Some(p, v), last._1._1, last._1._2, last._2)
                    case ((None, None), last) => QueueState(None, last._1._1, last._1._2, last._2)
                    case _ => throw new Exception("cannot happen")
                }

            val onChangedLocally =
                mockFunction[(QueueState, QueueState, core.Time), Unit]("onChangedLocally")

            toQueueState(localQueue) and toQueueState(localQueueOpposite) += {
                case (q,p) => onChangedLocally(q, p, core.Scheduler.currentTime)
            }

            val onChangedRemotely =
                mockFunction[(QueueState, QueueState, core.Time), Unit]("onChangedRemotely")

            toQueueState(remoteQueue) and toQueueState(remoteQueueOpposite) += {
                case (q,p) => onChangedRemotely(q, p, core.Scheduler.currentTime)
            }

            def expected(q : QueueState, p : QueueState) = {
                onChangedLocally expects (q,p,after(up)) once ()
                onChangedRemotely expects(q,p, after(up_down)) once ()
            }

            def checkLocalResult(expected: LevelInfo*)(expectedOpposite: LevelInfo*) = {
                checkResultImpl(side)(Some(localQueue.bestLevel), expected.toList)
                checkResultImpl(side.opposite)(Some(localQueueOpposite.bestLevel), expectedOpposite.toList)
            }

            class OrderPlaced(val price: Ticks, val volume: Quantity) {
                val signedPrice = price signed side
                val events = new ListenerWithTime(s"$price.$volume", up_down)
                val canceller = new Canceller
                remoteBook process LimitOrder(side, price, volume, events, Some(canceller))

                def Traded(volume : Quantity, incoming : ListenerWithTime) =
                    events Traded (signedPrice, volume, incoming)

                def Cancelled(amount : Quantity) =
                    events Cancelled amount

                def Completed() = events Completed ()
            }

            def after(dt : core.Duration) = core.Scheduler.currentTime + dt

            val V1 = 9

            expected(E.levels((initialPrice, V1)), E)

            val _1 = new OrderPlaced(initialPrice, V1)

            scheduler advance up

            assert(Remote.delayedListenersCount == 1)

            checkLocalResult(LevelInfo(_1.signedPrice, _1.volume :: Nil))()

            scheduler advance down + epsilon

            val Incoming = new ListenerWithTime("Incoming", up_down)
        }

        s"OrderBook($side)" should s"be constructed properly with one $side order" in new Initial {}

        it should "allow cancel small part of order" in new Initial {

            val C1 = 5
            assert(C1 < V1)

            _1.events Cancelled C1

            expected(E.levels((initialPrice, V1 - C1)), E)

            remoteBook cancel(_1.canceller, C1)

            scheduler advance up_down + epsilon

            checkLocalResult(LevelInfo(_1.signedPrice, _1.volume - C1 :: Nil))()
        }

        it should "allow cancel order completely" in new Initial {
            _1.events Cancelled _1.volume Completed()
            expected(E, E)
            remoteBook cancel(_1.canceller, _1.volume)
            scheduler advance up
            checkLocalResult()()
            scheduler advance down + epsilon
        }

        it should "allow cancel more than unmatched amount of order" in new Initial {
            _1.events Cancelled _1.volume Completed()
            expected(E, E)
            remoteBook cancel(_1.canceller, _1.volume + 5)
            scheduler advance up
            checkLocalResult()()
            scheduler advance down + epsilon
        }

        it should "accept orders of the same price" in new Initial {

            val V2 = 8
            expected(E levels ((initialPrice, V1 + V2)), E)

            val _2 = new OrderPlaced(initialPrice, V2)

            scheduler advance up

            assert(Remote.delayedListenersCount == 2)

            checkLocalResult(LevelInfo(_1.signedPrice, _1.volume :: _2.volume :: Nil))()

            scheduler advance down + epsilon
        }

        it should "match with limit orders having small price" in new Initial {

            val c1 = 5
            assert(c1 < _1.volume)

            _1 Traded (c1, Incoming)
            Incoming Completed()

            expected(E levels ((initialPrice, V1 - c1)) trades ((initialPrice, c1)), E)

            remoteBook process LimitOrder(side.opposite, initialPrice, c1, Incoming)

            scheduler advance up

            checkLocalResult(LevelInfo(_1.signedPrice, _1.volume - c1 :: Nil))()

            scheduler advance down + epsilon
        }

        it should "match with market orders having small price" in new Initial {

            val c1 = 5
            assert(c1 < _1.volume)

            _1 Traded (c1, Incoming)
            Incoming Completed()

            expected(E levels ((initialPrice, V1 - c1)) trades ((initialPrice, c1)), E)

            remoteBook process MarketOrder(side.opposite, c1, Incoming)

            scheduler advance up_down + epsilon

            checkLocalResult(LevelInfo(_1.signedPrice, _1.volume - c1 :: Nil))()
        }

        it should "put limit orders with too small price into another queue" in new Initial {

            val c1 = 5
            assert(c1 < _1.volume)

            val incomingPrice = _1.signedPrice moreAggressiveBy 1

            expected(E levels ((initialPrice, V1)), E levels ((incomingPrice.ticks, c1)))

            remoteBook process LimitOrder(side.opposite, incomingPrice.ticks, c1, Incoming)

            scheduler advance up_down + epsilon

            checkLocalResult(LevelInfo(_1.signedPrice, _1.volume :: Nil))(LevelInfo(incomingPrice.opposite, c1 :: Nil))
        }

        class WithMoreAggressive extends Initial {
            val moreAggressivePrice = _1.signedPrice moreAggressiveBy 3

            val V2 = 8
            expected(E levels ((moreAggressivePrice.ticks, V2), (initialPrice, fetchVolume - V2)), E)

            val _2 = new OrderPlaced(moreAggressivePrice.ticks, V2)

            scheduler advance up

            checkLocalResult(LevelInfo(_2.signedPrice, _2.volume :: Nil), LevelInfo(_1.signedPrice, _1.volume :: Nil))()

            scheduler advance down + epsilon
        }

        it should "accept orders of more aggressive price" in new WithMoreAggressive {}

        it should "match first order completely with a limit order having too big volume but not very aggressive price" in new WithMoreAggressive {

            val c1 = _1.volume + _2.volume + 5

            val slightlyMoreAggressivePrice = _1.signedPrice moreAggressiveBy 1

            _2 Traded (_2.volume, Incoming) Completed()

            expected(
                E levels ((initialPrice, V1)) trades ((moreAggressivePrice.ticks, _2.volume)),
                E levels ((slightlyMoreAggressivePrice.ticks, c1 - _2.volume)))

            remoteBook process LimitOrder(side.opposite, slightlyMoreAggressivePrice.ticks, c1, Incoming)

            scheduler advance up_down + epsilon

            checkLocalResult(LevelInfo(_1.signedPrice, _1.volume :: Nil))(LevelInfo(slightlyMoreAggressivePrice.opposite, c1 - _2.volume :: Nil))
        }

        it should "match completely with limit orders having too big volume and not aggressive price" in new WithMoreAggressive {

            val c1 = _1.volume + _2.volume + 5

            val notAggressivePrice = _1.signedPrice lessAggressiveBy 1

            _1 Traded (_1.volume, Incoming) Completed()
            _2 Traded (_2.volume, Incoming) Completed()

            expected(
                E trades((_1.price, _1.volume), (_2.price, _2.volume)),
                E levels ((notAggressivePrice.ticks, c1 - _2.volume - _1.volume)))

            remoteBook process LimitOrder(side.opposite, notAggressivePrice.ticks, c1, Incoming)

            scheduler advance up_down + epsilon

            checkLocalResult()(LevelInfo(notAggressivePrice.opposite, c1 - _2.volume - _1.volume :: Nil))
        }

        it should "match completely with market orders having too big volume" in new WithMoreAggressive {

            val c1 = _1.volume + _2.volume + 5

            _1 Traded (_1.volume, Incoming) Completed()
            _2 Traded (_2.volume, Incoming) Completed()

            Incoming Cancelled c1 - _1.volume - _2.volume Completed()

            expected(E trades((_1.price, _1.volume), (_2.price, _2.volume)), E)

            remoteBook process MarketOrder(side.opposite, c1, Incoming)

            scheduler advance up_down + epsilon

            checkLocalResult()()
        }
    }
}