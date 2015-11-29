package marketsim
package trader

import marketsim.orderbook.{Cancelled, Traded, AbstractOrder}
import marketsim.orderbook.linear._
import marketsim.reactive.Unary
import marketsim.ops._


class MarketClientSpec extends marketsim.orderbook.linear.common.Base {

    Side.choices foreach { side =>

        class Initial {
            val scheduler = core.Scheduler.recreate()

            val tickMapper = new orderbook.LinearMapper(cents(1))
            val initialPrice = Ticks(100)

            val priceCurrency = tickMapper toCurrency initialPrice

            val localBook = new Book(tickMapper)

            val up = Duration(3)
            val down = Duration(5)
            val up_down = up + down
            val epsilon = Duration(1)

            val remoteBook = new Remote.Book(localBook, up, down)

            case class State(inventory : Quantity, balance : Currency)

            class Trader(label : String)
            {
                val client = new MarketClient(remoteBook)
                val observableState =
                    Unary(new Inventory(client) and new Balance(client), label + ".toState"){ case (i,b) => State(i,b) }


                var orderSent : AbstractOrder = null  // it will keep the last sent order

                val onTraded = mockFunction[Traded,Unit](label + ".onTraded")
                val onCancelled = mockFunction[Cancelled, Unit](label + ".onCancelled")
                val orderTraded = mockFunction[(AbstractOrder, Traded), Unit](label + ".orderTraded")
                val orderCancelled = mockFunction[(AbstractOrder, Quantity), Unit](label + ".orderCancelled")
                val orderCompleted = mockFunction[AbstractOrder, Unit](label + ".orderCompleted")

                client.orderSent += { order => orderSent = order }

                client.onTraded += onTraded
                client.onCancelled += onCancelled

                client.orderTraded += orderTraded
                client.orderCancelled += orderCancelled
                client.orderCompleted += orderCompleted
            }

            val A = new Trader("A")
            val B = new Trader("B")

            val stateChanged = mockFunction[(State, State), Unit]("stateChanged")

            A.observableState and B.observableState += stateChanged

            val V1 = 9

            val token = A.client sendCancellableLimitOrder (side, initialPrice, V1)

            def step(): Unit = {
                scheduler advance up_down + epsilon
            }

            step()
        }

        s"Existing $side order " should "be partly cancelled" in new Initial {

            val C = V1 - 1

            A.onCancelled expects Cancelled(side, C)
            A.orderCancelled expects (A.orderSent, C)

            A.client cancel (token, C)

            step()
        }

        it should "be completely cancelled" in new Initial {
            val C = V1 + 1

            A.onCancelled expects Cancelled(side, V1)
            A.orderCancelled expects (A.orderSent, V1)
            A.orderCompleted expects A.orderSent

            A.client cancel (token, C)

            step()

            A.client cancel (token, C)  // nop

        }

        it should "match partly with incoming small market order" in new Initial {

            val C = V1 - 1

            A.onTraded expects Traded(initialPrice signed side, C)
            B.onTraded expects Traded(initialPrice signed side.opposite, C)

            A.orderTraded expects (A.orderSent, Traded(initialPrice signed side, C))

            stateChanged expects (
                State(side makeSigned C, (side makeSigned priceCurrency) * C),
                State(side.opposite makeSigned C, (side.opposite makeSigned priceCurrency) * C))

            B.client sendMarketOrder (side.opposite, C)

            B.orderTraded expects (B.orderSent, Traded(initialPrice signed side.opposite, C))
            B.orderCompleted expects B.orderSent

            step()
        }

        it should "match completely with incoming big market order" in new Initial {

            val C = V1 + 1

            A.onTraded expects Traded(initialPrice signed side, V1)
            B.onTraded expects Traded(initialPrice signed side.opposite, V1)
            B.onCancelled expects Cancelled(side.opposite, C - V1)

            A.orderTraded expects (A.orderSent, Traded(initialPrice signed side, V1))
            A.orderCompleted expects A.orderSent

            stateChanged expects (
                State(side makeSigned V1, (side makeSigned priceCurrency) * V1),
                State(side.opposite makeSigned V1, (side.opposite makeSigned priceCurrency) * V1))

            B.client sendMarketOrder (side.opposite, C)

            B.orderTraded expects (B.orderSent, Traded(initialPrice signed side.opposite, V1))
            B.orderCancelled expects (B.orderSent, C - V1)
            B.orderCompleted expects B.orderSent

            step()
        }

        it should "match partly with incoming small limit order" in new Initial {

            val C = V1 - 1

            A.onTraded expects Traded(initialPrice signed side, C)
            B.onTraded expects Traded(initialPrice signed side.opposite, C)

            A.orderTraded expects (A.orderSent, Traded(initialPrice signed side, C))

            stateChanged expects (
                State(side makeSigned C, (side makeSigned priceCurrency) * C),
                State(side.opposite makeSigned C, (side.opposite makeSigned priceCurrency) * C))

            B.client sendLimitOrder (side.opposite, initialPrice, C)

            B.orderTraded expects (B.orderSent, Traded(initialPrice signed side.opposite, C))
            B.orderCompleted expects B.orderSent

            step()
        }

        it should "match completely with incoming big limit order" in new Initial {

            val C = V1 + 1

            A.onTraded expects Traded(initialPrice signed side, V1)
            B.onTraded expects Traded(initialPrice signed side.opposite, V1)

            A.orderTraded expects (A.orderSent, Traded(initialPrice signed side, V1))
            A.orderCompleted expects A.orderSent

            stateChanged expects (
                State(side makeSigned V1, (side makeSigned priceCurrency) * V1),
                State(side.opposite makeSigned V1, (side.opposite makeSigned priceCurrency) * V1))

            B.client sendLimitOrder (side.opposite, initialPrice, C)

            B.orderTraded expects (B.orderSent, Traded(initialPrice signed side.opposite, V1))

            step()
        }
    }
}
