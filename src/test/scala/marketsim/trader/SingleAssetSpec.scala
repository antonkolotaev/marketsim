package marketsim
package trader

import marketsim.orderbook.linear._

class SingleAssetSpec extends marketsim.orderbook.linear.common.Base {

    Side.choices foreach { side =>

        class Initial {
            val scheduler = core.Scheduler.recreate()
            Remote.recreateDelayedListeners()

            val tickMapper = new LinearMapper(cents(1))
            val initialPrice = Ticks(100)

            val localBook = new Book(tickMapper)

            val up = Duration(3)
            val down = Duration(5)
            val up_down = up + down
            val epsilon = Duration(1)

            val remoteBook = new Remote.Book(localBook, up, down)
            val remoteQueue = remoteBook queue side
            val remoteQueueOpposite = remoteBook queue side.opposite


        }

    }
}
