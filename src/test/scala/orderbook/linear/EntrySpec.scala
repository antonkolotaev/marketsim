package orderbook.linear

class EntrySpec extends common.Base {

    class Initial {
        val V = 9
        val L = new Listener("Initial")
        val e = new Entry(V, L)

        assert(!e.fulfilled)
        assert(e.unmatchedVolume == V)
    }

    "Entry" should "be non-empty when constructed" in new Initial {}

    it should "allow cancel a little bit" in new Initial {

        val C = 5
        L.onCancelled expects C once()
        assert((e cancel C) == C)

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

    class Matching extends Initial {
        val P = SignedTicks(97)
        val incoming = new Listener("Incoming")

        def expectTrade(amount : Quantity) = {
            L.onTraded expects (P.abs, amount) once()
            incoming.onTraded expects (P.abs, amount) once()
        }
    }

    it should "trade with small orders" in new Matching {
        val C = 5
        expectTrade(C)

        assert((e matchWith (P, C, incoming)) == 5)

        assert(!e.fulfilled)
        assert(e.unmatchedVolume == V - C)
    }

    it should "allow trade all amount" in new Matching {

        expectTrade(V)
        L.onCompleted expects() once()
        assert((e matchWith (P,V, incoming)) == V)

        assert(e.fulfilled)
        assert(e.unmatchedVolume == 0)
    }

    it should "allow trade more than all amount" in new Matching {

        expectTrade(V)
        L.onCompleted expects() once()
        assert((e matchWith (P,V + 10, incoming)) == V)

        assert(e.fulfilled)
        assert(e.unmatchedVolume == 0)
    }



}
