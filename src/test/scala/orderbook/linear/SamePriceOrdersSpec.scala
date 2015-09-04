package orderbook.linear

class SamePriceOrdersSpec extends common.Base {

    class Initial {
        val level = new SamePriceOrders(100)

        def check(volumes : Quantity*) = {
            val actual = level.ownOrders map { _.unmatchedVolume }
            assert(actual.toList == volumes.toList)
        }

        assert(level.totalVolume == 0)
        check()

        val v1 = 9

        val L1 = new Listener

        val e1 = level storeImpl (v1, L1)

        assert(level.totalVolume == v1)
        check(v1)

        val v2 = 7
        val L2 = new Listener

        val e2 = level storeImpl (v2, L2)

        assert(level.totalVolume == v1 + v2)
        check(v1, v2)
    }

    "same price orders" should "enqueue different orders" in new Initial {}

}
