package marketsim {
package orderbook {
package linear {

    import common._

    class BookSpec extends Base {

        Side.choices foreach { side =>

            class Initial {

                val scheduler = core.Scheduler.recreate()
                val epsilon = Duration(1)

                def step() = scheduler advance epsilon

                val tickMapper = new LinearMapper(cents(1))
                val initialPrice = Ticks(100)

                val book = new Book(tickMapper)
                book fetchPriceLevelsTillVolume 10

                val queue = book queue side
                val queueOpposite = book queue side.opposite

                case class LevelDescription(inTicks: Ticks, inCurrency: USD, volume: Quantity) {
                    override def toString = s"$volume@$inTicks"
                }

                def fmt[X](xs: List[X]) = xs mkString("[", ",", "]")

                case class QueueState(levels: List[LevelDescription]) {

                    def levels(levels: (Ticks, Quantity)*) =
                        copy(levels = levels.toList map { case (t, v) => LevelDescription(t, tickMapper toCurrency t, v min 10) })

                    override def toString = s"{ levels = ${fmt(levels)} }"
                }

                val E = QueueState(Nil)

                import marketsim.ops._

                def toQueueState(queue: Queue[USD]) =
                    reactive.Unary(queue.priceLevels, "toQueue") {
                        levels => QueueState(levels map { case (p, c, v) => LevelDescription(p, c, v) })
                    }

                val onChanged =
                    mockFunction[(QueueState, QueueState), Unit]("onChanged")

                val onTraded =
                    mockFunction[TradeDone, Unit]("onTraded")

                queue.tradeDone += onTraded

                toQueueState(queue) and toQueueState(queueOpposite) += onChanged

                def checkResult(expected: LevelInfo*)(expectedOpposite: LevelInfo*) = {
                    checkResultImpl(side)(Some(queue.bestLevel), expected.toList)
                    checkResultImpl(side.opposite)(Some(queueOpposite.bestLevel), expectedOpposite.toList)
                }

                def expected(q: QueueState, p: QueueState, trades: (Ticks, Quantity)*) = {
                    onChanged expects(q, p) once()
                    trades foreach { t =>
                        onTraded expects TradeDone(t._1 signed side, t._2) once()
                    }
                }


                class OrderPlaced(val price: Ticks, val volume: Quantity) {
                    val signedPrice = price signed side
                    val events = new Listener(s"$price.$volume")
                    val canceller = new Canceller
                    book process LimitOrder(side, price, volume, events, Some(canceller))
                    step()

                    def Traded(v: Quantity, incoming: Listener) = {
                        events Traded(signedPrice, v, incoming); this
                    }

                    def Cancelled(c: Quantity) = {
                        events Cancelled c; this
                    }

                    def Completed() = {
                        events Completed(); this
                    }
                }

                onChanged expects(E.levels((initialPrice, 9)), E) once()

                val _1 = new OrderPlaced(initialPrice, 9)

                checkResult(LevelInfo(_1.signedPrice, _1.volume :: Nil))()
            }

            s"OrderBook($side)" should s"be constructed properly with one $side order" in new Initial {}

            it should "allow cancel small part of order" in new Initial {

                _1 Cancelled 5

                expected(E.levels((initialPrice, 9 - 5)), E)

                book cancel(_1.canceller, 5)
                step()
                checkResult(LevelInfo(_1.signedPrice, _1.volume - 5 :: Nil))()
            }

            it should "allow cancel order completely" in new Initial {
                _1 Cancelled _1.volume Completed()
                expected(E, E)
                book cancel(_1.canceller, _1.volume)
                step()
                checkResult()()
            }

            it should "allow cancel more than unmatched amount of order" in new Initial {
                _1 Cancelled _1.volume Completed()
                expected(E, E)
                book cancel(_1.canceller, _1.volume + 5)
                step()
                checkResult()()
            }

            it should "accept orders of the same price" in new Initial {

                expected(E levels ((initialPrice, 9 + 8)), E)

                val _2 = new OrderPlaced(initialPrice, 8)

                checkResult(LevelInfo(_1.signedPrice, _1.volume :: _2.volume :: Nil))()
            }

            it should "match with limit orders having small price" in new Initial {

                val Incoming = new Listener("Incoming")
                val c1 = 5
                assert(c1 < _1.volume)

                _1 Traded(c1, Incoming)
                Incoming Completed()

                expected(E levels ((initialPrice, 9 - 5)), E, (initialPrice, 5))

                book process LimitOrder(side.opposite, initialPrice, c1, Incoming)
                step()

                checkResult(LevelInfo(_1.signedPrice, _1.volume - c1 :: Nil))()
            }

            it should "match with market orders having small price" in new Initial {

                val Incoming = new Listener("Incoming")
                val c1 = 5
                assert(c1 < _1.volume)

                _1 Traded(c1, Incoming)
                Incoming Completed()

                expected(E levels ((initialPrice, 9 - 5)), E, (initialPrice, 5))

                book process MarketOrder(side.opposite, c1, Incoming)
                step()

                checkResult(LevelInfo(_1.signedPrice, _1.volume - c1 :: Nil))()
            }

            it should "put limit orders with too small price into another queue" in new Initial {

                val Incoming = new Listener("Incoming")
                val c1 = 5
                assert(c1 < _1.volume)

                val incomingPrice = _1.signedPrice moreAggressiveBy 1

                expected(E levels ((initialPrice, 9)), E levels ((incomingPrice.ticks, 5)))

                book process LimitOrder(side.opposite, incomingPrice.ticks, c1, Incoming)
                step()

                checkResult(LevelInfo(_1.signedPrice, _1.volume :: Nil))(LevelInfo(incomingPrice.opposite, c1 :: Nil))
            }

            class WithMoreAggressive extends Initial {
                val moreAggressivePrice = _1.signedPrice moreAggressiveBy 3

                expected(E levels((moreAggressivePrice.ticks, 8), (initialPrice, 2)), E)

                val _2 = new OrderPlaced(moreAggressivePrice.ticks, 8)

                checkResult(LevelInfo(_2.signedPrice, _2.volume :: Nil), LevelInfo(_1.signedPrice, _1.volume :: Nil))()
            }

            it should "accept orders of more aggressive price" in new WithMoreAggressive {}

            it should "match first order completely with a limit order having too big volume but not very aggressive price" in new WithMoreAggressive {

                val Incoming = new Listener("Incoming")
                val c1 = _1.volume + _2.volume + 5

                val slightlyMoreAggressivePrice = _1.signedPrice moreAggressiveBy 1

                _2 Traded(_2.volume, Incoming) Completed()

                expected(
                    E levels ((initialPrice, 9)),
                    E levels ((slightlyMoreAggressivePrice.ticks, c1 - _2.volume)),
                    (moreAggressivePrice.ticks, _2.volume))

                book process LimitOrder(side.opposite, slightlyMoreAggressivePrice.ticks, c1, Incoming)
                step()

                checkResult(LevelInfo(_1.signedPrice, _1.volume :: Nil))(LevelInfo(slightlyMoreAggressivePrice.opposite, c1 - _2.volume :: Nil))
            }

            it should "match completely with limit orders having too big volume and not aggressive price" in new WithMoreAggressive {

                val Incoming = new Listener("Incoming")
                val c1 = _1.volume + _2.volume + 5

                val notAggressivePrice = _1.signedPrice lessAggressiveBy 1

                _1 Traded(_1.volume, Incoming) Completed()
                _2 Traded(_2.volume, Incoming) Completed()

                expected(
                    E,
                    E levels ((notAggressivePrice.ticks, c1 - _2.volume - _1.volume)),
                    (_1.price, _1.volume), (_2.price, _2.volume))

                book process LimitOrder(side.opposite, notAggressivePrice.ticks, c1, Incoming)
                step()

                checkResult()(LevelInfo(notAggressivePrice.opposite, c1 - _2.volume - _1.volume :: Nil))
            }

            it should "match completely with market orders having too big volume" in new WithMoreAggressive {

                val Incoming = new Listener("Incoming")
                val c1 = _1.volume + _2.volume + 5

                _1 Traded(_1.volume, Incoming) Completed()
                _2 Traded(_2.volume, Incoming) Completed()
                Incoming Cancelled c1 - _1.volume - _2.volume Completed()

                expected(E, E, (_1.price, _1.volume), (_2.price, _2.volume))

                book process MarketOrder(side.opposite, c1, Incoming)
                step()

                checkResult()()
            }


        }

    }

}}}