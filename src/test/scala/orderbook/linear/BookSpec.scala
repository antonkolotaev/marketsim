package orderbook.linear

import orderbook.linear.common._

class BookSpec extends Base {

    Side.choices foreach { side =>

        class Initial {

            val unsignedPrice = 100
            val initialPrice = side makeSigned unsignedPrice

            val book = new Book()

            val queue = book queue side

            def checkResult(expected: LevelInfo*) =
                checkResultImpl(side)(Some(queue.bestLevel), expected.toList)

            val v1 = 9
            val v2 = 8

            val events1 = new Listener("1")
            val cancellation1 = book process LimitOrder(side, unsignedPrice, v1, events1)

            checkResult(LevelInfo(initialPrice, v1 :: Nil))
        }

        "OrderBook" should s"be constructed properly with one $side order" in new Initial {}

    }

}
