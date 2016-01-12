package marketsim
package ops

import ops.Implicits._
import conversions.Implicits._

class MinusSpec extends EnsureChanges {

    "Some(1) - Some(2) - Some(3)" should "be Some(-5)" in assertResult(Some(1) - Some(2) - Some(3))(Some(-5))
    "Some(1) - None" should "be None" in assertResult(Some(1) - None)(None)

    "Function[Option[Double]] - Function[Int]" should "be a Function[Option[Double]]" in {

        var a = some(1.0)
        var b = 2
        var c = some(3)
        val A = () => a
        val B = () => b
        val C = () => c

        val R = A - B - C
        val R1 = A - B - C
        assert(R eq R1)

        def changeA(x : Option[Double], expected : Option[Double]) = (() => a = x, expected)
        def changeB(x : Int,            expected : Option[Double]) = (() => b = x, expected)
        def changeC(x : Option[Int],    expected : Option[Double]) = (() => c = x, expected)

        ensureFunction(R, Some(-5.0),
            changeA(None, None),
            changeA(some(9), some(4.0)),
            changeB(5, some(1.0)),
            changeC(None, None),
            changeC(some(2), some(2.0))
        )
    }

    "Signal[Option[Double]] - Signal[Int]" should "be a Signal[Option[Double]]" in {

        val A = new reactive.Variable(some(1.0), "A")
        val B = new reactive.Variable(2, "B")
        val C = new reactive.Variable(some(3), "C")

        val R = A - B - C
        val Ri = A - B - 3
        val R1 = A - B - C
        assert(R eq R1)

        def changeA(x : Option[Double], expected : Option[Double]) = (() => A :=! x, expected)
        def changeB(x : Int,            expected : Option[Double]) = (() => B :=! x, expected)
        def changeC(x : Option[Int],    expected : Option[Double]) = (() => C :=! x, expected)

        ensureSignal(R, Some(-5.0),
            changeA(None, None),
            changeA(some(9), some(4.0)),
            changeA(some(1.0), some(-5.0)),
            changeB(5, some(-8.0)),
            changeB(2, some(-5.0)),
            changeC(None, None),
            changeC(some(2), some(-4.0)),
            changeC(some(3), some(-5.0)))

        ensureSignal(Ri, Some(-5.0),
            changeA(None, None),
            changeA(some(9), some(4.0)),
            changeA(some(1.0), some(-5.0)),
            changeB(5, some(-8.0)),
            changeB(2, some(-5.0)))
    }
}