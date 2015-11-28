package marketsim
package trader

import marketsim.orderbook.{Cancelled, Traded, AbstractOrder}
import marketsim.orderbook.linear._

class MarketClientSpec extends marketsim.orderbook.linear.common.Base {

    Side.choices foreach { side =>

        class Initial {
            val scheduler = core.Scheduler.recreate()

            val tickMapper = new orderbook.LinearMapper(cents(1))
            val initialPrice = Ticks(100)

            val localBook = new Book(tickMapper)

            val up = Duration(3)
            val down = Duration(5)
            val up_down = up + down
            val epsilon = Duration(1)

            val remoteBook = new Remote.Book(localBook, up, down)

            val client = new MarketClient(remoteBook)

            var orderSent : AbstractOrder = null  // it will keep the last sent order

            val onTraded = mockFunction[Traded,Unit]("onTraded")
            val onCancelled = mockFunction[Cancelled, Unit]("onCancelled")
            val orderTraded = mockFunction[(AbstractOrder, Traded), Unit]("orderTraded")
            val orderCancelled = mockFunction[(AbstractOrder, Quantity), Unit]("orderCancelled")
            val orderCompleted = mockFunction[AbstractOrder, Unit]("orderCompleted")

            client.orderSent += { order => orderSent = order }

            client.onTraded += onTraded
            client.onCancelled += onCancelled

            client.orderTraded += orderTraded
            client.orderCancelled += orderCancelled
            client.orderCompleted += orderCompleted

            val V1 = 9

            val token = client sendCancellableLimitOrder (side, initialPrice, V1)

            def step(): Unit = {
                scheduler advance up_down + epsilon
            }

            val original = orderSent

            step()
        }

        s"Existing $side order " should "be partly cancelled" in new Initial {

            val C = V1 - 1

            onCancelled expects Cancelled(side, C)
            orderCancelled expects (original, C)

            client cancel (token, C)

            step()
        }

        it should "be completely cancelled" in new Initial {
            val C = V1 + 1

            onCancelled expects Cancelled(side, V1)
            orderCancelled expects (original, V1)
            orderCompleted expects original

            client cancel (token, C)

            step()

            client cancel (token, C)

        }

        it should "match partly with incoming small market order" in new Initial {

            val C = V1 - 1

            onTraded expects Traded(initialPrice signed side, C)
            onTraded expects Traded(initialPrice signed side.opposite, C)

            orderTraded expects (original, Traded(initialPrice signed side, C))

            client sendMarketOrder (side.opposite, C)

            orderTraded expects (orderSent, Traded(initialPrice signed side.opposite, C))
            orderCompleted expects orderSent

            step()
        }

        it should "match completely with incoming big market order" in new Initial {

            val C = V1 + 1

            onTraded expects Traded(initialPrice signed side, V1)
            onTraded expects Traded(initialPrice signed side.opposite, V1)
            onCancelled expects Cancelled(side.opposite, C - V1)

            orderTraded expects (original, Traded(initialPrice signed side, V1))
            orderCompleted expects original

            client sendMarketOrder (side.opposite, C)

            orderTraded expects (orderSent, Traded(initialPrice signed side.opposite, V1))
            orderCancelled expects (orderSent, C - V1)
            orderCompleted expects orderSent

            step()
        }

        it should "match partly with incoming small limit order" in new Initial {

            val C = V1 - 1

            onTraded expects Traded(initialPrice signed side, C)
            onTraded expects Traded(initialPrice signed side.opposite, C)

            orderTraded expects (original, Traded(initialPrice signed side, C))

            client sendLimitOrder (side.opposite, initialPrice, C)

            orderTraded expects (orderSent, Traded(initialPrice signed side.opposite, C))
            orderCompleted expects orderSent

            step()
        }

        it should "match completely with incoming big limit order" in new Initial {

            val C = V1 + 1

            onTraded expects Traded(initialPrice signed side, V1)
            onTraded expects Traded(initialPrice signed side.opposite, V1)

            orderTraded expects (original, Traded(initialPrice signed side, V1))
            orderCompleted expects original

            client sendLimitOrder (side.opposite, initialPrice, C)

            orderTraded expects (orderSent, Traded(initialPrice signed side.opposite, V1))

            step()
        }
    }
}
