package exchange.simple

import org.scalatest.FlatSpec

class OrderQueueSpec extends FlatSpec {

    val emptyListener = new OrderListener {}

    class Initial(side : Side) {

        val initialPrice = side makeSigned 100

        val q = new PriceLevel(initialPrice, None, None)

        assert(q.totalVolume == 0)
        assert(q.price == initialPrice)
        assert(q.side == side)
        assert(q.allOrders.isEmpty)

        val v1 = 9
        val v2 = 8

        q store (LimitOrder(side, initialPrice, v1), emptyListener)

        assert(q.totalVolume == v1)
        assert(q.price == initialPrice)
        assert(q.side == side)
        assert(q.allOrders.toList == LimitOrderInfo(side, initialPrice, v1, emptyListener) :: Nil)
    }
    
    Side.choices foreach { side =>
        s"PriceLevel($side)" should "be constructed properly with one order" in new Initial(side) {}

        it should "accept orders of the same price" in new Initial(side) {

            q store (LimitOrder(side, initialPrice, v2), emptyListener)

            assert(q.totalVolume == v1 + v2)
            assert(q.price == initialPrice)
            assert(q.side == side)
            assert(q.allOrders.toList == LimitOrderInfo(side, initialPrice, v1, emptyListener) ::
                                         LimitOrderInfo(side, initialPrice, v2, emptyListener) :: Nil)
        }

        it should "accept orders of more aggressive price" in new Initial(side) {

            val moreAggressivePrice = initialPrice - 3

            q store (LimitOrder(side, moreAggressivePrice, v2), emptyListener)

            assert(q.totalVolume == v1)
            assert(q.price == initialPrice)
            assert(q.side == side)
            assert(q.allOrders.toList == LimitOrderInfo(side, initialPrice, v1, emptyListener) :: Nil)

            val p = q.getPrevious.get

            assert(p.totalVolume == v2)
            assert(p.price == moreAggressivePrice)
            assert(p.side == side)
            assert(p.allOrders.toList == LimitOrderInfo(side, moreAggressivePrice, v2, emptyListener) ::
                                         LimitOrderInfo(side, initialPrice, v1, emptyListener) :: Nil)
        }

        class WithNext(side : Side) extends Initial(side) {

            val lessAggressivePrice = initialPrice + 5

            q store (LimitOrder(side, lessAggressivePrice, v2), emptyListener)

            assert(q.totalVolume == v1)
            assert(q.price == initialPrice)
            assert(q.side == side)
            assert(q.allOrders.toList == LimitOrderInfo(side, initialPrice, v1, emptyListener) ::
                                         LimitOrderInfo(side, lessAggressivePrice, v2, emptyListener) :: Nil)

            val p = q.getNext.get

            assert(p.totalVolume == v2)
            assert(p.price == lessAggressivePrice)
            assert(p.side == side)
            assert(p.allOrders.toList == LimitOrderInfo(side, lessAggressivePrice, v2, emptyListener)  :: Nil)
        }

        it should "accept orders of less aggressive price" in new WithNext(side) {}

        it should "accept orders of less aggressive price when there are following levels" in new WithNext(side) {

            val slightlyLessAggressivePrice = initialPrice + 1

            val v3 = 7

            q store (LimitOrder(side, slightlyLessAggressivePrice, v3), emptyListener)

            assert(q.totalVolume == v1)
            assert(q.price == initialPrice)
            assert(q.side == side)
            assert(q.allOrders.toList == LimitOrderInfo(side, initialPrice, v1, emptyListener) ::
                                         LimitOrderInfo(side, slightlyLessAggressivePrice, v3, emptyListener) ::
                                         LimitOrderInfo(side, lessAggressivePrice, v2, emptyListener) :: Nil)

            val r = q.getNext.get

            assert(r.totalVolume == v3)
            assert(r.price == slightlyLessAggressivePrice)
            assert(r.side == side)
            assert(r.allOrders.toList == LimitOrderInfo(side, slightlyLessAggressivePrice, v3, emptyListener) ::
                                         LimitOrderInfo(side, lessAggressivePrice, v2, emptyListener) :: Nil)

            val s = r.getNext.get

            assert(s.totalVolume == v2)
            assert(s.price == lessAggressivePrice)
            assert(s.side == side)
            assert(s.allOrders.toList == LimitOrderInfo(side, lessAggressivePrice, v2, emptyListener) :: Nil)
        }
    }

}
