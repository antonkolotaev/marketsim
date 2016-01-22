package marketsim
package ops

import marketsim.ops.Implicits._

class DivSpec extends EnsureChanges {

    def div[A,B,R](a : A, b : B)(implicit m : Div.OnUnbound[A,B,R]) = m div(a,b)

    "9 / 4 / 2 (div)" should "be 9 / 4 / 2" in assertResult(div(div(9, 4), 2))(9 / 4 / 2)
    "9.5 / 4 / 2  (div)" should "be 3.5" in assertResult(div(div(9.5, 4), 2))(9.5 / 4 / 2)
    "9 / 4.5 / 2  (div)" should "be 2.5" in assertResult(div(div(9, 4.5), 2))(9 / 4.5 / 2)
    "9.5 / 4.5 / 2  (div)" should "be 3" in assertResult(div(div(9.5, 4.5), 2))(9.5 / 4.5 / 2)

    "9 / 4 / 2" should "be 9 / 4 / 2" in assertResult(9 / 4 / 2)(9 / 4 / 2)
    "9.5 / 4 / 2" should "be 9.5 / 4 / 2" in assertResult(9.5 / 4 / 2)(9.5 / 4 / 2)
    "9 / 4.5 / 2" should "be 9 / 4.5 / 2" in assertResult(9 / 4.5 / 2)(9 / 4.5 / 2)
    "9.5 / 4.5 / 2" should "be 9.5 / 4.5 / 2" in assertResult(9.5 / 4.5 / 2)(9.5 / 4.5 / 2)

    "Some(9.5) / Some(4.5) / Some(2)" should "be Some(3)" in assertResult(Some(9.5) / Some(4.5) / Some(2))(Some(9.5 / 4.5 / 2))
    "Some(9.5) / 4.5 / Some(2)" should "be Some(3)" in assertResult(Some(9.5) / 4.5 / Some(2))(Some(9.5 / 4.5 / 2))
    "9.5 / Some(4.5) / Some(2)" should "be Some(3)" in assertResult(9.5 / Some(4.5) / Some(2))(Some(9.5 / 4.5 / 2))

    "None / 2 / None" should "be None" in assertResult(none[Double] / 2 / none[Int])(None)
    "None / 3 / Some(2)" should "be None" in assertResult(none[Double] / 3 / some(2))(None)

    "Signal[Option[Double]] / Signal[Int]" should "be a Signal[Option[Double]]" in {

        val a = new reactive.Variable(some(24.0), "A")
        val A = a //: reactive.Signal[Option[Double]]
        val b = new reactive.Variable(2, "B")
        val B = b //: reactive.Signal[Int]
        val c = new reactive.Variable(some(3), "C")
        val C = c //: reactive.Signal[Option[Int]]

        val R = A / B / C
        val Ri = A / B / 3
        val iR = Some(1.0) / B
        val R1 = A / B / C
        assert(R eq R1)

        def changeA = change[Option[Double], Option[Double]](a)_
        def changeB = change[Int, Option[Double]](b)_
        def changeC = change[Option[Int], Option[Double]](c)_

        ensureSignal(R, Some(24.0 / 2 / 3),
            changeA(None, None),
            changeA(some(9), some(9.0 / 2 / 3)),
            changeA(some(24.0), some(24.0 / 2 / 3)),
            changeB(5, some(24.0 / 5 / 3)),
            changeB(2, some(24.0 / 2 / 3)),
            changeC(None, None),
            changeC(some(2), some(24.0 / 2 / 2)),
            changeC(some(3), some(24.0 / 2 / 3)))

        ensureSignal(Ri, Some(24.0 / 2 / 3),
            changeA(None, None),
            changeA(some(9), some(9.0 / 2 / 3)),
            changeA(some(24.0), some(24.0 / 2 / 3)),
            changeB(5, some(24.0 / 5 / 3)),
            changeB(2, some(24.0 / 2 / 3)))

        ensureSignal(iR, Some(1.0 / 2),
            changeB(5, some(1.0 / 5)),
            changeB(2, some(1.0 / 2)))
    }

    class FunctionsAndSignals
    {
        val a = new reactive.Variable(some(24.0), "A")
        val A = a : () => Option[Double]
        val As = a //: reactive.Signal[Option[Double]]
        val b = new reactive.Variable(2, "B")
        val B = b : () => Int
        val Bs = b //: reactive.Signal[Int]
        val c = new reactive.Variable(some(3), "C")
        val C = c : () => Option[Int]
        val Cs = c //: reactive.Signal[Option[Int]]

        def changeA = change[Option[Double], Option[Double]](a)_
        def changeB = change[Int, Option[Double]](b)_
        def changeC = change[Option[Int], Option[Double]](c)_
    }

    "() => Option[Double]] / () => Int" should "be a () => Option[Double]" in new FunctionsAndSignals {

        val R = A / B / C
        val Ri = A / B / 3
        val iR = Some(1.0) / B
        val R1 = A / B / C
        assert(R eq R1)

        ensureFunction(R, Some(24.0 / 2 / 3),
            changeA(None, None),
            changeA(some(9), some(9.0 / 2 / 3)),
            changeA(some(24.0), some(24.0 / 2 / 3)),
            changeB(5, some(24.0 / 5 / 3)),
            changeB(2, some(24.0 / 2 / 3)),
            changeC(None, None),
            changeC(some(2), some(24.0 / 2 / 2)),
            changeC(some(3), some(24.0 / 2 / 3)))

        ensureFunction(Ri, Some(24.0 / 2 / 3),
            changeA(None, None),
            changeA(some(9), some(9.0 / 2 / 3)),
            changeA(some(24.0), some(24.0 / 2 / 3)),
            changeB(5, some(24.0 / 5 / 3)),
            changeB(2, some(24.0 / 2 / 3)))

        ensureFunction(iR, Some(1.0 / 2),
            changeB(5, some(1.0 / 5)),
            changeB(2, some(1.0 / 2)))
    }

    "() => Option[Double]] / reactive.Signal[Int]" should "be a () => Option[Double]" in new FunctionsAndSignals {

        val Rs = As / B / C
        val Ris = A / Bs / 3
        val R1s = As / B / C
        assert(Rs eq R1s)

        ensureFunction(Rs, Some(24.0 / 2 / 3),
            changeA(None, None),
            changeA(some(9), some(9.0 / 2 / 3)),
            changeA(some(24.0), some(24.0 / 2 / 3)),
            changeB(5, some(24.0 / 5 / 3)),
            changeB(2, some(24.0 / 2 / 3)),
            changeC(None, None),
            changeC(some(2), some(24.0 / 2 / 2)),
            changeC(some(3), some(24.0 / 2 / 3)))

        ensureFunction(Ris, Some(24.0 / 2 / 3),
            changeA(None, None),
            changeA(some(9), some(9.0 / 2 / 3)),
            changeA(some(24.0), some(24.0 / 2 / 3)),
            changeB(5, some(24.0 / 5 / 3)),
            changeB(2, some(24.0 / 2 / 3)))
    }

    "Unbound[() => Option[Double]] / Unbound[reactive.Signal[Int]]" should "be a Unbound[() => Option[Double]]" in new FunctionsAndSignals {

        val Rs = unbound(As) / B / unbound(C)
        val Ris = A / unbound(Bs) / 3
        val R1s = unbound(As) / unbound(B) / unbound(C)
        assert(Rs eq R1s)

        ensureFunction(Rs(ctx), Some(24.0 / 2 / 3),
            changeA(None, None),
            changeA(some(9), some(9.0 / 2 / 3)),
            changeA(some(24.0), some(24.0 / 2 / 3)),
            changeB(5, some(24.0 / 5 / 3)),
            changeB(2, some(24.0 / 2 / 3)),
            changeC(None, None),
            changeC(some(2), some(24.0 / 2 / 2)),
            changeC(some(3), some(24.0 / 2 / 3)))

        ensureFunction(Ris(ctx), Some(24.0 / 2 / 3),
            changeA(None, None),
            changeA(some(9), some(9.0 / 2 / 3)),
            changeA(some(24.0), some(24.0 / 2 / 3)),
            changeB(5, some(24.0 / 5 / 3)),
            changeB(2, some(24.0 / 2 / 3)))
    }
}
