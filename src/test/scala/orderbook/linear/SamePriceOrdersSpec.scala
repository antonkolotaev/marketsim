package orderbook.linear

class SamePriceOrdersSpec extends common.Base {
    
    class Initial {
        
        val P = SignedTicks(100)
        val level = new SamePriceOrders(P)

        class OrderStored(val volume : Quantity)
        {
            val listener = new Listener(volume.toString)
            val canceller = new Canceller
            level storeImpl (volume, listener, Some(canceller))
        }

        def check(volumes : Quantity*) = {
            val actual = level.ownOrders map { _.unmatchedVolume }
            assert(actual.toList == volumes.toList)
        }

        assert(level.totalVolume == 0)
        check()

        val _1 = new OrderStored(9)

        assert(level.totalVolume == _1.volume)
        check(_1.volume)

        val _2 = new OrderStored(7)

        assert(level.totalVolume == _1.volume + _2.volume)
        check(_1.volume, _2.volume)
    }

    "same price orders" should "enqueue different orders" in new Initial {}

    it should "cancel a small part of order" in new Initial {

        val c1 = 5
        assert(c1 < _1.volume)

        _1.listener.onCancelled expects c1 once ()

        assert(_1.canceller(c1) == c1)

        assert(level.totalVolume == _1.volume + _2.volume - c1)
        check(_1.volume - c1, _2.volume)
    }

    it should "cancel an order completely" in new Initial {

        val c1 = _1.volume

        _1.listener.onCancelled expects c1 once ()
        _1.listener.onCompleted expects () once ()

        assert(_1.canceller(c1) == c1)

        assert(level.totalVolume == _1.volume + _2.volume - c1)
        check(_1.volume - c1, _2.volume)
    }

    it should "cancel more than order volume" in new Initial {

        val c1 = _1.volume + 5

        _1.listener.onCancelled expects _1.volume once ()
        _1.listener.onCompleted expects () once ()

        assert(_1.canceller(c1) == _1.volume)

        assert(level.totalVolume == _2.volume)
        check(0, _2.volume)
    }

    it should "match with small orders" in new Initial {

        val c1 = 5
        assert(c1 < _1.volume)

        val Incoming = new Listener("Incoming")

        _1.listener.onTraded expects (P, c1) once ()
        Incoming.onTraded expects (P.opposite, c1) once ()
        assert(level.matchImpl(c1, Incoming) == 0)

        assert(level.totalVolume == _1.volume + _2.volume - c1)
        check(_1.volume - c1, _2.volume)
    }

    it should "match completely the first order with an order of the same size" in new Initial {

        val c1 = _1.volume

        val Incoming = new Listener("Incoming")

        _1.listener.onTraded expects (P, c1) once ()
        _1.listener.onCompleted expects () once ()
        Incoming.onTraded expects (P.opposite, c1) once ()

        assert(level.matchImpl(c1, Incoming) == 0)

        assert(level.totalVolume == _1.volume + _2.volume - c1)
        check(_2.volume)
    }

    it should "match all queue with a very big order" in new Initial {

        val c1 = _1.volume + _2.volume + 5

        val Incoming = new Listener("Incoming")

        _1.listener.onTraded expects (P, _1.volume) once ()
        _1.listener.onCompleted expects () once ()

        _2.listener.onTraded expects (P, _2.volume) once ()
        _2.listener.onCompleted expects () once ()

        Incoming.onTraded expects (P.opposite, _1.volume) once ()
        Incoming.onTraded expects (P.opposite, _2.volume) once ()

        assert(level.matchImpl(c1, Incoming) == c1 - _1.volume - _2.volume)

        assert(level.totalVolume == 0)
        check()
    }
}
