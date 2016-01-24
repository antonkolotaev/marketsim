package marketsim
package ops

class AndSpec extends EnsureChanges {

    import Implicits._

    def and[A,B,R](a : A, b : B)(implicit m : And.OnUnbound[A,B,R]) = m and(a,b)

    "true && false && true (and)" should "be false" in assertResult(and(and(true, false), true))(false)
    "false && false && true (and)" should "be false" in assertResult(and(and(false, false), true))(false)
    "true && true && true (and)" should "be false" in assertResult(and(and(true, true), true))(true)

    "Some(true) && Some(true) && Some(true)" should "be Some(true)" in assertResult(Some(true) && Some(true) && Some(true))(Some(true))
    "Some(true) && Some(false) && Some(true)" should "be Some(false)" in assertResult(Some(true) && Some(false) && Some(true))(Some(false))
    "Some(true) && true && Some(true)" should "be Some(true)" in assertResult(Some(true) && true && Some(true))(Some(true))
    "Some(true) && false && Some(true)" should "be Some(false)" in assertResult(Some(true) && false && Some(true))(Some(false))
    "true && Some(true) && Some(true)" should "be Some(true)" in assertResult(true && Some(true) && Some(true))(Some(true))
    "false && Some(true) && Some(true)" should "be Some(false)" in assertResult(false && Some(true) && Some(true))(Some(false))

    "None && true && None" should "be None" in assertResult(none[Boolean] && true && none[Boolean])(None)
    "None && false && Some(false)" should "be None" in assertResult(none[Boolean] && false && some(false))(None)

    "Signal[Option[Boolean]] && Signal[Boolean]" should "be a Signal[Option[Boolean]]" in {

        val a = new reactive.Variable(some(true), "A")
        val A = a //: reactive.Signal[Option[Double]]
        val b = new reactive.Variable(true, "B")
        val B = b //: reactive.Signal[Int]
        val c = new reactive.Variable(some(true), "C")
        val C = c //: reactive.Signal[Option[Int]]

        val R = A && B && C
        val Ri = A && B && true
        val iR = Some(true) && B
        val R1 = A && B && C
        assert(R eq R1)

        def changeA = change[Option[Boolean], Option[Boolean]](a)_
        def changeB = change[Boolean, Option[Boolean]](b)_
        def changeC = change[Option[Boolean], Option[Boolean]](c)_

        ensureSignal(R, Some(true),
            changeA(None, None),
            changeA(some(false), some(false)),
            changeA(some(true), some(true)),
            changeB(false, some(false)),
            changeB(true, some(true)),
            changeC(None, None),
            changeC(some(false), some(false)),
            changeC(some(true), some(true)))

        ensureSignal(Ri, Some(true),
            changeA(None, None),
            changeA(some(false), some(false)),
            changeA(some(true), some(true)),
            changeB(false, some(false)),
            changeB(true, some(true)))

        ensureSignal(iR, Some(true),
            changeB(false, some(false)),
            changeB(true, some(true)))
    }

    class FunctionsAndSignals
    {
        val a = new reactive.Variable(some(true), "A")
        val A = a : () => Option[Boolean]
        val As = a //: reactive.Signal[Option[Double]]
        val b = new reactive.Variable(true, "B")
        val B = b : () => Boolean
        val Bs = b //: reactive.Signal[Int]
        val c = new reactive.Variable(some(true), "C")
        val C = c : () => Option[Boolean]
        val Cs = c //: reactive.Signal[Option[Int]]

        def changeA = change[Option[Boolean], Option[Boolean]](a)_
        def changeB = change[Boolean, Option[Boolean]](b)_
        def changeC = change[Option[Boolean], Option[Boolean]](c)_
    }

    "() => Option[Boolean]] && () => Boolean" should "be a () => Option[Boolean]" in new FunctionsAndSignals {

        val R = A && B && C
        val Ri = A && B && true
        val iR = Some(true) && B
        val R1 = A && B && C
        assert(R eq R1)

        ensureFunction(R, Some(true),
            changeA(None, None),
            changeA(some(false), some(false)),
            changeA(some(true), some(true)),
            changeB(false, some(false)),
            changeB(true, some(true)),
            changeC(None, None),
            changeC(some(false), some(false)),
            changeC(some(true), some(true)))

        ensureFunction(Ri, Some(true),
            changeA(None, None),
            changeA(some(false), some(false)),
            changeA(some(true), some(true)),
            changeB(false, some(false)),
            changeB(true, some(true)))

        ensureFunction(iR, Some(true),
            changeB(false, some(false)),
            changeB(true, some(true)))
    }
    
    "() => Option[Boolean]] / reactive.Signal[Boolean]" should "be a () => Option[Boolean]" in new FunctionsAndSignals {

        val Rs = As && B && C
        val Ris = A && Bs && true
        val R1s = As && B && C
        assert(Rs eq R1s)

        ensureFunction(Rs, Some(true),
            changeA(None, None),
            changeA(some(false), some(false)),
            changeA(some(true), some(true)),
            changeB(false, some(false)),
            changeB(true, some(true)),
            changeC(None, None),
            changeC(some(false), some(false)),
            changeC(some(true), some(true)))

        ensureFunction(Ris, Some(true),
            changeA(None, None),
            changeA(some(false), some(false)),
            changeA(some(true), some(true)),
            changeB(false, some(false)),
            changeB(true, some(true)))
    }
    
    "Unbound[() => Option[Boolean]] && Unbound[reactive.Signal[Booean]]" should "be a Unbound[() => Option[Boolean]]" in new FunctionsAndSignals {
    
        val Rs = unbound(As) && B && unbound(C)
        val Ris = A && unbound(Bs) && true
        val R1s = unbound(As) && unbound(B) && unbound(C)
        assert(Rs eq R1s)
    
        ensureFunction(Rs(ctx), Some(true),
            changeA(None, None),
            changeA(some(false), some(false)),
            changeA(some(true), some(true)),
            changeB(false, some(false)),
            changeB(true, some(true)),
            changeC(None, None),
            changeC(some(false), some(false)),
            changeC(some(true), some(true)))
    
        ensureFunction(Ris(ctx), Some(true),
            changeA(None, None),
            changeA(some(false), some(false)),
            changeA(some(true), some(true)),
            changeB(false, some(false)),
            changeB(true, some(true)))
    } 
}
