package marketsim
package ops

class OrSpec extends EnsureChanges {

    import Implicits._

    def or[A,B,R](a : A, b : B)(implicit m : Or.OnUnbound[A,B,R]) = m or(a,b)

    "false || true || false (or)" should "be true" in assertResult(or(or(false, true), false))(true)
    "true || true || false (or)" should "be true" in assertResult(or(or(true, true), false))(true)
    "false || false || false (or)" should "be false" in assertResult(or(or(false, false), false))(false)

    "Some(false) || Some(false) || Some(false)" should "be Some(false)" in assertResult(Some(false) || Some(false) || Some(false))(Some(false))
    "Some(false) || Some(true) || Some(false)" should "be Some(true)" in assertResult(Some(false) || Some(true) || Some(false))(Some(true))
    "Some(false) || false || Some(false)" should "be Some(false)" in assertResult(Some(false) || false || Some(false))(Some(false))
    "Some(false) || true || Some(false)" should "be Some(true)" in assertResult(Some(false) || true || Some(false))(Some(true))
    "false || Some(false) || Some(false)" should "be Some(false)" in assertResult(false || Some(false) || Some(false))(Some(false))
    "true || Some(false) || Some(false)" should "be Some(true)" in assertResult(true || Some(false) || Some(false))(Some(true))

    "None || false || None" should "be None" in assertResult(none[Boolean] || false || none[Boolean])(None)
    "None || true || Some(true)" should "be None" in assertResult(none[Boolean] || true || some(true))(None)

    "Signal[Option[Boolean]] || Signal[Boolean]" should "be a Signal[Option[Boolean]]" in {

        val a = new reactive.Variable(some(false), "A")
        val A = a //: reactive.Signal[Option[Double]]
        val b = new reactive.Variable(false, "B")
        val B = b //: reactive.Signal[Int]
        val c = new reactive.Variable(some(false), "C")
        val C = c //: reactive.Signal[Option[Int]]

        val R = A || B || C
        val Ri = A || B || false
        val iR = Some(false) || B
        val R1 = A || B || C
        assert(R eq R1)

        def changeA = change[Option[Boolean], Option[Boolean]](a)_
        def changeB = change[Boolean, Option[Boolean]](b)_
        def changeC = change[Option[Boolean], Option[Boolean]](c)_

        ensureSignal(R, Some(false),
            changeA(None, None),
            changeA(some(true), some(true)),
            changeA(some(false), some(false)),
            changeB(true, some(true)),
            changeB(false, some(false)),
            changeC(None, None),
            changeC(some(true), some(true)),
            changeC(some(false), some(false)))

        ensureSignal(Ri, Some(false),
            changeA(None, None),
            changeA(some(true), some(true)),
            changeA(some(false), some(false)),
            changeB(true, some(true)),
            changeB(false, some(false)))

        ensureSignal(iR, Some(false),
            changeB(true, some(true)),
            changeB(false, some(false)))
    }

    class FunctionsOrSignals
    {
        val a = new reactive.Variable(some(false), "A")
        val A = a : () => Option[Boolean]
        val As = a //: reactive.Signal[Option[Double]]
    val b = new reactive.Variable(false, "B")
        val B = b : () => Boolean
        val Bs = b //: reactive.Signal[Int]
    val c = new reactive.Variable(some(false), "C")
        val C = c : () => Option[Boolean]
        val Cs = c //: reactive.Signal[Option[Int]]

        def changeA = change[Option[Boolean], Option[Boolean]](a)_
        def changeB = change[Boolean, Option[Boolean]](b)_
        def changeC = change[Option[Boolean], Option[Boolean]](c)_
    }

    "() => Option[Boolean]] || () => Boolean" should "be a () => Option[Boolean]" in new FunctionsOrSignals {

        val R = A || B || C
        val Ri = A || B || false
        val iR = Some(false) || B
        val R1 = A || B || C
        assert(R eq R1)

        ensureFunction(R, Some(false),
            changeA(None, None),
            changeA(some(true), some(true)),
            changeA(some(false), some(false)),
            changeB(true, some(true)),
            changeB(false, some(false)),
            changeC(None, None),
            changeC(some(true), some(true)),
            changeC(some(false), some(false)))

        ensureFunction(Ri, Some(false),
            changeA(None, None),
            changeA(some(true), some(true)),
            changeA(some(false), some(false)),
            changeB(true, some(true)),
            changeB(false, some(false)))

        ensureFunction(iR, Some(false),
            changeB(true, some(true)),
            changeB(false, some(false)))
    }

    "() => Option[Boolean]] / reactive.Signal[Boolean]" should "be a () => Option[Boolean]" in new FunctionsOrSignals {

        val Rs = As || B || C
        val Ris = A || Bs || false
        val R1s = As || B || C
        assert(Rs eq R1s)

        ensureFunction(Rs, Some(false),
            changeA(None, None),
            changeA(some(true), some(true)),
            changeA(some(false), some(false)),
            changeB(true, some(true)),
            changeB(false, some(false)),
            changeC(None, None),
            changeC(some(true), some(true)),
            changeC(some(false), some(false)))

        ensureFunction(Ris, Some(false),
            changeA(None, None),
            changeA(some(true), some(true)),
            changeA(some(false), some(false)),
            changeB(true, some(true)),
            changeB(false, some(false)))
    }

    "Unbound[() => Option[Boolean]] || Unbound[reactive.Signal[Booean]]" should "be a Unbound[() => Option[Boolean]]" in new FunctionsOrSignals {

        val Rs = unbound(As) || B || unbound(C)
        val Ris = A || unbound(Bs) || false
        val R1s = unbound(As) || unbound(B) || unbound(C)
        assert(Rs eq R1s)

        ensureFunction(Rs(ctx), Some(false),
            changeA(None, None),
            changeA(some(true), some(true)),
            changeA(some(false), some(false)),
            changeB(true, some(true)),
            changeB(false, some(false)),
            changeC(None, None),
            changeC(some(true), some(true)),
            changeC(some(false), some(false)))

        ensureFunction(Ris(ctx), Some(false),
            changeA(None, None),
            changeA(some(true), some(true)),
            changeA(some(false), some(false)),
            changeB(true, some(true)),
            changeB(false, some(false)))
    }
}
