package trader

import orderbook.linear._

class SingleAssetSpec extends orderbook.linear.common.Base {

    Side.choices foreach { side =>

        class Initial {
            val scheduler = core.Scheduler.recreate()
            Remote.recreateDelayedListeners()

            val tickMapper = new LinearMapper(cents(1))
            val initialPrice = Ticks(100)

            val localBook = new Book(tickMapper)

            val up = core.Duration(3)
            val down = core.Duration(5)
            val up_down = up + down
            val epsilon = core.Duration(1)

            val remoteBook = new Remote.Book(localBook, up, down)
            val remoteQueue = remoteBook queue side
            val remoteQueueOpposite = remoteBook queue side.opposite


        }

    }
}