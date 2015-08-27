package orderbook.linear

class EntrySpec extends common.Base {

    class Initial {
        val V = 9
        val L = new Listener
        val e = new Entry(V, L)

        assert(!e.fulfilled)
        assert(e.unmatchedVolume == V)
    }

    "Entry" should "be non-empty when constructed" in new Initial {}

    it should "allow cancel a little bit" in new Initial {

        val C = 5
        L.onCancelled expects C once()
        assert((e cancel C) == 5)

        assert(!e.fulfilled)
        assert(e.unmatchedVolume == V - C)
    }

    it should "allow cancel all amount" in new Initial {

        L.onCancelled expects V once()
        L.onCompleted expects () once()
        assert((e cancel V) == V)

        assert(e.fulfilled)
        assert(e.unmatchedVolume == 0)
    }

    it should "allow cancel more than all amount" in new Initial {

        val C = V + 10
        L.onCancelled expects V once()
        L.onCompleted expects () once()
        assert((e cancel V) == V)

        assert(e.fulfilled)
        assert(e.unmatchedVolume == 0)
    }
}
