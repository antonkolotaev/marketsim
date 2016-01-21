package marketsim
package ops

import marketsim.ops.Implicits._

class MinusExSpec extends EnsureChanges {

    def minus[A,B,R](a : A, b : B)(implicit m : OnUnbound[A,B,R]) = m minus(a,b)

    "9 - 4 - 2 (minus)" should "be 3" in assertResult(minus(minus(9, 4), 2))(3)
    "9.5 - 4 - 2  (minus)" should "be 3.5" in assertResult(minus(minus(9.5, 4), 2))(3.5)
    "9 - 4.5 - 2  (minus)" should "be 2.5" in assertResult(minus(minus(9, 4.5), 2))(2.5)
    "9.5 - 4.5 - 2  (minus)" should "be 3" in assertResult(minus(minus(9.5, 4.5), 2))(3.0)

    "9 - 4 - 2" should "be 3" in assertResult(9 - 4 - 2)(3)
    "9.5 -. 4 - 2" should "be 3.5" in assertResult(9.5 - 4 - 2)(3.5)
    "9 - 4.5 - 2" should "be 2.5" in assertResult(9 - 4.5 - 2)(2.5)
    "9.5 - 4.5 - 2" should "be 3" in assertResult(9.5 - 4.5 - 2)(3.0)

    "Some(9.5) - Some(4.5) - Some(2)" should "be Some(3)" in assertResult(Some(9.5) - Some(4.5) - Some(2))(Some(3.0))
    "Some(9.5) - 4.5 - Some(2)" should "be Some(3)" in assertResult(Some(9.5) - 4.5 - Some(2))(Some(3.0))
    "9.5 - Some(4.5) - Some(2)" should "be Some(3)" in assertResult(9.5 - Some(4.5) - Some(2))(Some(3.0))

    "None - 2 - None" should "be None" in assertResult(none[Double] - 2 - none[Int])(None)
    "None - 3 - Some(2)" should "be None" in assertResult(none[Double] - 3 - some(2))(None)

    "Signal[Option[Double]] - Signal[Int]" should "be a Signal[Option[Double]]" in {

        val a = new reactive.Variable(some(1.0), "A")
        val A = a : reactive.Signal[Option[Double]]
        val b = new reactive.Variable(2, "B")
        val B = b : reactive.Signal[Int]
        val c = new reactive.Variable(some(3), "C")
        val C = c : reactive.Signal[Option[Int]]

        val R = A - B - C
        val Ri = A - B - 3
        val iR = Some(1.0) - B
        val R1 = A - B - C
        assert(R eq R1)

        def changeA(x : Option[Double], expected : Option[Double]) = (() => a :=! x, expected)
        def changeB(x : Int,            expected : Option[Double]) = (() => b :=! x, expected)
        def changeC(x : Option[Int],    expected : Option[Double]) = (() => c :=! x, expected)

        ensureSignal(R, Some(-4.0),
            changeA(None, None),
            changeA(some(9), some(4.0)),
            changeA(some(1.0), some(-4.0)),
            changeB(5, some(-7.0)),
            changeB(2, some(-4.0)),
            changeC(None, None),
            changeC(some(2), some(-3.0)),
            changeC(some(3), some(-4.0)))

        ensureSignal(Ri, Some(-4.0),
            changeA(None, None),
            changeA(some(9), some(4.0)),
            changeA(some(1.0), some(-4.0)),
            changeB(5, some(-7.0)),
            changeB(2, some(-4.0)))

        ensureSignal(iR, Some(-1.0),
            changeB(5, some(-4.0)),
            changeB(2, some(-1.0)))
    }

    class FunctionsAndSignals
    {
        val a = new reactive.Variable(some(1.0), "A")
        val A = a : () => Option[Double]
        val As = a : reactive.Signal[Option[Double]]
        val b = new reactive.Variable(2, "B")
        val B = b : () => Int
        val Bs = b : reactive.Signal[Int]
        val c = new reactive.Variable(some(3), "C")
        val C = c : () => Option[Int]
        val Cs = c : reactive.Signal[Option[Int]]

        def changeA(x : Option[Double], expected : Option[Double]) = (() => a :=! x, expected)
        def changeB(x : Quantity,            expected : Option[Double]) = (() => b :=! x, expected)
        def changeC(x : Option[Quantity],    expected : Option[Double]) = (() => c :=! x, expected)
    }

    "() => Option[Double]] - () => Int" should "be a () => Option[Double]" in new FunctionsAndSignals {

        val R = A - B - C
        val Ri = A - B - 3
        val iR = Some(1.0) - B
        val R1 = A - B - C
        assert(R eq R1)

        ensureFunction(R, Some(-4.0),
            changeA(None, None),
            changeA(some(9), some(4.0)),
            changeA(some(1.0), some(-4.0)),
            changeB(5, some(-7.0)),
            changeB(2, some(-4.0)),
            changeC(None, None),
            changeC(some(2), some(-3.0)),
            changeC(some(3), some(-4.0)))

        ensureFunction(Ri, Some(-4.0),
            changeA(None, None),
            changeA(some(9), some(4.0)),
            changeA(some(1.0), some(-4.0)),
            changeB(5, some(-7.0)),
            changeB(2, some(-4.0)))

        ensureFunction(iR, Some(-1.0),
            changeB(5, some(-4.0)),
            changeB(2, some(-1.0)))
    }

    "() => Option[Double]] - reactive.Signal[Int]" should "be a () => Option[Double]" in new FunctionsAndSignals {

        val Rs = As - B - C
        val Ris = A - Bs - 3
        val R1s = As - B - C
        assert(Rs eq R1s)

        ensureFunction(Rs, Some(-4.0),
            changeA(None, None),
            changeA(some(9), some(4.0)),
            changeA(some(1.0), some(-4.0)),
            changeB(5, some(-7.0)),
            changeB(2, some(-4.0)),
            changeC(None, None),
            changeC(some(2), some(-3.0)),
            changeC(some(3), some(-4.0)))

        ensureFunction(Ris, Some(-4.0),
            changeA(None, None),
            changeA(some(9), some(4.0)),
            changeA(some(1.0), some(-4.0)),
            changeB(5, some(-7.0)),
            changeB(2, some(-4.0)))
    }

    "Unbound[() => Option[Double]] - Unbound[reactive.Signal[Int]]" should "be a Unbound[() => Option[Double]]" in new FunctionsAndSignals {

        val Rs = unbound(As) - B - unbound(C)
        val Ris = A - unbound(Bs) - 3
        val R1s = unbound(As) - unbound(B) - unbound(C)
        assert(Rs eq R1s)

        ensureFunction(Rs(ctx), Some(-4.0),
            changeA(None, None),
            changeA(some(9), some(4.0)),
            changeA(some(1.0), some(-4.0)),
            changeB(5, some(-7.0)),
            changeB(2, some(-4.0)),
            changeC(None, None),
            changeC(some(2), some(-3.0)),
            changeC(some(3), some(-4.0)))

        ensureFunction(Ris(ctx), Some(-4.0),
            changeA(None, None),
            changeA(some(9), some(4.0)),
            changeA(some(1.0), some(-4.0)),
            changeB(5, some(-7.0)),
            changeB(2, some(-4.0)))
    }
}
