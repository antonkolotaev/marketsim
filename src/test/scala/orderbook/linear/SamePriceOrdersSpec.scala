package orderbook.linear

class SamePriceOrdersSpec extends common.Base {

    class Initial {

        val P = 100
        val level = new SamePriceOrders(P)

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

    it should "cancel a small part of order" in new Initial {

        val c1 = 5
        assert(c1 < v1)

        L1.onCancelled expects c1 once ()

        assert(e1(c1) == c1)

        assert(level.totalVolume == v1 + v2 - c1)
        check(v1 - c1, v2)
    }

    it should "cancel an order completely" in new Initial {

        val c1 = v1

        L1.onCancelled expects c1 once ()
        L1.onCompleted expects () once ()

        assert(e1(c1) == c1)

        assert(level.totalVolume == v1 + v2 - c1)
        check(v1 - c1, v2)
    }

    it should "cancel more than order volume" in new Initial {

        val c1 = v1 + 5

        L1.onCancelled expects v1 once ()
        L1.onCompleted expects () once ()

        assert(e1(c1) == v1)

        assert(level.totalVolume == v2)
        check(0, v2)
    }

    it should "match with small orders" in new Initial {

        val c1 = 5
        assert(c1 < v1)

        val Incoming = new Listener

        L1.onTraded expects (P, c1) once ()
        Incoming.onTraded expects (P, c1) once ()
        assert(level.matchImpl(c1, Incoming) == 0)

        assert(level.totalVolume == v1 + v2 - c1)
        check(v1 - c1, v2)
    }

    it should "match completely the first order with an order of the same size" in new Initial {

        val c1 = v1

        val Incoming = new Listener

        L1.onTraded expects (P, c1) once ()
        L1.onCompleted expects () once ()
        Incoming.onTraded expects (P, c1) once ()

        assert(level.matchImpl(c1, Incoming) == 0)

        assert(level.totalVolume == v1 + v2 - c1)
        check(v2)
    }

    it should "match all queue with a very big order" in new Initial {

        val c1 = v1 + v2 + 5

        val Incoming = new Listener

        L1.onTraded expects (P, v1) once ()
        L1.onCompleted expects () once ()

        L2.onTraded expects (P, v2) once ()
        L2.onCompleted expects () once ()

        Incoming.onTraded expects (P, v1) once ()
        Incoming.onTraded expects (P, v2) once ()

        assert(level.matchImpl(c1, Incoming) == c1 - v1 - v2)

        assert(level.totalVolume == 0)
        check()
    }
}
