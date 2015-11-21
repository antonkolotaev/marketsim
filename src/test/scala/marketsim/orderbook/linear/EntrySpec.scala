package marketsim
package orderbook
package linear

class EntrySpec extends common.Base {

    class Initial {
        val V = 9
        val L = new Listener("Initial")
        val order = new LimitOrder(Sell, Ticks(100), V, L, None)
        val e = new Entry(order, V, L)

        assert(!e.fulfilled)
        assert(e.unmatchedVolume == V)

        def cancelled(L : Listener, amount : Quantity) = 
            L.onCancelled expects amount once()
        
        def completed(L : Listener) = 
            L.onCompleted expects () once ()

        def traded(L : Listener, price : SignedTicks, amount : Quantity, incoming : Listener) = {
            L.onTraded expects Traded(price, amount) once()
            incoming.onTraded expects Traded(price.opposite, amount) once()
        }
    }

    "Entry" should "be non-empty when constructed" in new Initial {}

    it should "allow cancel a little bit" in new Initial {

        val C = 5
        L Cancelled C
        assert((e cancel (Sell, C)) == C)

        assert(!e.fulfilled)
        assert(e.unmatchedVolume == V - C)
    }

    it should "allow cancel all amount" in new Initial {

        L Cancelled V Completed ()
        assert((e cancel (Sell, V)) == V)

        assert(e.fulfilled)
        assert(e.unmatchedVolume == 0)
    }

    it should "allow cancel more than all amount" in new Initial {

        val C = V + 10
        L Cancelled V Completed ()

        assert((e cancel (Sell, V)) == V)

        assert(e.fulfilled)
        assert(e.unmatchedVolume == 0)
    }

    class Matching extends Initial {
        val P = SignedTicks(97)
        val incoming = new Listener("Incoming")
    }

    it should "trade with small orders" in new Matching {
        val C = 5
        L Traded (P, C, incoming)

        assert((e matchWith (P, C, incoming)) == 5)

        assert(!e.fulfilled)
        assert(e.unmatchedVolume == V - C)
    }

    it should "allow trade all amount" in new Matching {

        L Traded (P, V, incoming) Completed()
        assert((e matchWith (P,V, incoming)) == V)

        assert(e.fulfilled)
        assert(e.unmatchedVolume == 0)
    }

    it should "allow trade more than all amount" in new Matching {

        L Traded (P, V, incoming) Completed()
        assert((e matchWith (P,V + 10, incoming)) == V)

        assert(e.fulfilled)
        assert(e.unmatchedVolume == 0)
    }



}
