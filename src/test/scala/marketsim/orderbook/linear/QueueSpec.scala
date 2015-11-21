package marketsim {
package orderbook {
package linear {

    import common._

    class QueueSpec extends Base {

        Side.choices foreach { side =>

            class Initial {

                val scheduler = core.Scheduler.recreate()
                val epsilon = Duration(1)

                def step() = scheduler advance epsilon

                val initialPrice = Ticks(100) signed side

                val dummy = USD(0)
                val queue = new Queue(side, dummy)

                def checkResult(expected: LevelInfo*) =
                    checkResultImpl(side)(Some(queue.bestLevel), expected.toList)

                class OrderPlaced(val price: SignedTicks, val volume: Quantity) {
                    val events = new Listener(s"$price.$volume")
                    val canceller = new Canceller
                    queue store(price, dummy, volume, events, Some(canceller))

                    step()

                    def Traded(v: Quantity, incoming: Listener) = {
                        events Traded(price, v, incoming); this
                    }

                    def Cancelled(c: Quantity) = {
                        events Cancelled c; this
                    }

                    def Completed() = {
                        events Completed(); this
                    }
                }

                val _1 = new OrderPlaced(initialPrice, 9)

                checkResult(LevelInfo(initialPrice, _1.volume :: Nil))
            }

            s"OrderQueue($side)" should "be constructed properly with one order" in new Initial {}

            it should "allow cancel small part of order" in new Initial {
                _1 Cancelled 5
                queue cancel(_1.canceller, 5)
                step()
                checkResult(LevelInfo(initialPrice, _1.volume - 5 :: Nil))
            }

            it should "allow cancel order completely" in new Initial {
                _1 Cancelled _1.volume Completed()
                queue cancel(_1.canceller, _1.volume)
                step()
                checkResult()
            }

            it should "allow cancel more than unmatched amount of order" in new Initial {
                _1 Cancelled _1.volume Completed()
                queue cancel(_1.canceller, _1.volume + 5)
                step()
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

                _1 Traded(c1, Incoming)

                assert(queue.matchWith(initialPrice, c1, Incoming) == 0)
                step()
                checkResult(LevelInfo(initialPrice, _1.volume - c1 :: Nil))
            }

            it should "ignore orders with too small price" in new Initial {

                val Incoming = new Listener("Incoming")
                val c1 = 5
                assert(c1 < _1.volume)

                val incomingPrice = initialPrice moreAggressiveBy 1

                assert(queue.matchWith(incomingPrice, c1, Incoming) == c1)
                step()
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

                _1 Traded(_1.volume, Incoming) Completed()

                assert(queue.matchWith(initialPrice, _1.volume, Incoming) == 0)
                step()
                checkResult(LevelInfo(lessAggressivePrice, _2.volume :: Nil))
            }

            it should "match completely the first order with a big order having a less aggressive price" in new WithLessAggressive {

                val Incoming = new Listener("Incoming")

                val c = _1.volume + 7

                val p = initialPrice lessAggressiveBy 1
                assert(p isMoreAggressiveThan lessAggressivePrice)

                _1 Traded(_1.volume, Incoming) Completed()

                assert(queue.matchWith(p, c, Incoming) == c - _1.volume)
                step()
                checkResult(LevelInfo(lessAggressivePrice, _2.volume :: Nil))
            }

            it should "empty the queue being matched with a very big order" in new WithLessAggressive {

                val Incoming = new Listener("Incoming")

                val c = _1.volume + _2.volume + 5

                _1 Traded(_1.volume, Incoming) Completed()
                _2 Traded(_2.volume, Incoming) Completed()

                assert(queue.matchWith(MarketOrderPrice, c, Incoming) == c - _1.volume - _2.volume)
                step()
                checkResult()
            }

            class WithTwoLessAggressive extends WithLessAggressive {
                val slightlyLessAggressivePrice = initialPrice lessAggressiveBy 1

                val v3 = 7

                queue store(slightlyLessAggressivePrice, dummy, v3, emptyListener, None)
                step()

                checkResult(
                    LevelInfo(initialPrice, _1.volume :: Nil),
                    LevelInfo(slightlyLessAggressivePrice, v3 :: Nil),
                    LevelInfo(lessAggressivePrice, _2.volume :: Nil))
            }

            it should "accept orders of less aggressive price when there are following levels" in new WithTwoLessAggressive {}

        }

    }

}}}