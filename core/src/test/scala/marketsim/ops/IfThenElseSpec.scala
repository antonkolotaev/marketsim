package marketsim
package ops

class IfThenElseSpec extends EnsureChanges {

    import ops.Implicits._
    import conversions.Implicits._

    "IfThenElse of options" should "be an option" in {
        var C = some(false)
        var T = some(1)
        var E = some(9)
        def R = C Then T Else E
        val R1 = C Then T Else E
        // checking that Option[Double] doesn't interfere with Option[Int]
        val Td = some(1.0)
        val Ed = some(9.0)
        val Rd = C Then Td Else Ed
        val R1d = C Then Td Else Ed
        //assert(Rd eq R1d)
        //assert(R eq R1)

        assertResult(R)(Some(9))

        E = Some(7)
        assertResult(R)(Some(7))

        C = Some(true)
        assertResult(R)(Some(1))

        C = None
        assertResult(R)(None)

        C = Some(true)
        T = Some(3)
        assertResult(R)(Some(3))

        E = None
        assertResult(R)(Some(3))

        C = Some(false)
        assertResult(R)(None)
    }

    "IfThenElse of unbound options" should "be an unbound option" in {
        var c = some(false)
        var t = some(1)
        var e = some(9)
        def C = unbound(c)
        def T = unbound(t)
        def E = unbound(e)
        def R = C Then T Else E
        val R1 = C Then T Else E
        // checking that Option[Double] doesn't interfere with Option[Int]
        val Td = some(1.0)
        val Ed = some(9.0)
        val Rd = unbound(c) Then unbound(Td) Else unbound(Ed)
        val R1d = unbound(c) Then unbound(Td) Else unbound(Ed)
        assert(Rd eq R1d)
        assert(R eq R1)

        val ctx = new Context {}

        assertResult(R(ctx))(Some(9))

        e = Some(7)
        assertResult(E(ctx))(Some(7))
        assertResult(R(ctx))(Some(7))

        c = Some(true)
        assertResult(C(ctx))(Some(true))
        assertResult(T(ctx))(Some(1))
        assertResult(R(ctx))(Some(1))

        c = None
        assertResult(R(ctx))(None)

        c = Some(true)
        t = Some(3)
        assertResult(R(ctx))(Some(3))

        e = None
        assertResult(R(ctx))(Some(3))

        c = Some(false)
        assertResult(R(ctx))(None)
    }

    "IfThenElse of signals" should "be a signal" in {
        val C = new reactive.Variable(false, "C")
        val T = new reactive.Variable(1.0, "T")
        val E = new reactive.Variable(9, "E")
        val R = C Then T Else E
        val R1 = C Then T Else E
        // checking that Signal[Double] doesn't interfere with Signal[Int]
        val Td = new reactive.Variable(1.0, "T")
        val Ed = new reactive.Variable(9.0, "E")
        val Rd = C Then Td Else Ed
        val R1d = C Then Td Else Ed
        assert(Rd eq R1d)
        assert(R eq R1)
        def changeC(x : Boolean, expected : Double) = (() => C :=! x, expected)
        def changeT(x : Double, expected : Double) = (() => T :=! x, expected)
        def changeE(x : Int, expected : Double) = (() => E :=! x, expected)

        ensureSignal(R, 9.0, changeC(true, 1.0), changeT(3.5, 3.5), changeC(false, 9.0), changeE(7, 7.0))
    }

    "IfThenElse of option signals" should "be an option signal" in {
        val C = new reactive.Variable(some(false), "C")
        val T = new reactive.Variable(some(1.0), "T")
        val E = new reactive.Variable(some(9), "E")
        val R = C Then T Else E
        val R1 = C Then T Else E
        val Ri = C Then T Else 9
        // checking that Signal[Option[Double]] doesn't interfere with Signal[Option[Int]]
        val Td = new reactive.Variable(some(1.0), "T")
        val Ed = new reactive.Variable(some(9.0), "E")
        val Rd = C Then Td Else Ed
        val R1d = C Then Td Else Ed
        assert(Rd eq R1d)
        assert(R eq R1)
        def changeC(x : Option[Boolean], expected : Option[Double]) = (() => C :=! x, expected)
        def changeT(x : Option[Double], expected : Option[Double]) = (() => T :=! x, expected)
        def changeE(x : Option[Int], expected : Option[Double]) = (() => E :=! x, expected)

        ensureSignal(R, Some(9.0),
            changeC(Some(true), Some(1.0)) ,
            changeC(None, None),
            changeC(Some(true), Some(1.0)),
            changeT(Some(3.5), Some(3.5)),
            changeT(None, None),
            changeT(Some(1.0), Some(1.0)),
            changeC(Some(false), Some(9.0)),
            changeE(Some(7), Some(7.0)),
            changeE(None, None),
            changeE(Some(7), Some(7.0))
        )
        ensureSignal(Ri, Some(9.0),
            changeC(Some(true), Some(1.0)),
            changeC(None, None),
            changeC(Some(true), Some(1.0)),
            changeT(Some(3.5), Some(3.5)),
            changeT(None, None),
            changeT(Some(3.5), Some(3.5)),
            changeC(Some(false), Some(9.0))
        )
    }

    "IfThenElse of functions" should "be a function" in {
        val C = new reactive.Variable(false, "C")
        val c = C : () => Boolean
        val T = new reactive.Variable(1.0, "T")
        val E = new reactive.Variable(9, "E")
        val R = C Then T Else E
        val R1 = C Then T Else E
        // checking that () => Double doesn't interfere with () => Int
        val Td = new reactive.Variable(1.0, "T")
        val Ed = new reactive.Variable(9.0, "E")
        val Rd = c Then Td Else Ed
        val R1d = c Then Td Else Ed
        assert(Rd eq R1d)
        assert(R eq R1)
        def changeC(x : Boolean, expected : Double) = (() => C :=! x, expected)
        def changeT(x : Double, expected : Double) = (() => T :=! x, expected)
        def changeE(x : Int, expected : Double) = (() => E :=! x, expected)

        ensureFunction(R, 9.0, changeC(true, 1.0), changeT(3.5, 3.5), changeC(false, 9.0), changeE(7, 7.0))
    }

    "IfThenElse of option functions" should "be an option function" in {
        val C = new reactive.Variable(some(false), "C")
        val c = C : () => Option[Boolean]
        val T = new reactive.Variable(some(1.0), "T")
        val E = new reactive.Variable(some(9), "E")
        val Ei = 9
        val R = C Then T Else E
        val Ri = C Then T Else Ei
        val R1 = C Then T Else E
        // checking that () => Option[Double] doesn't interfere with () => Option[Int]
        val Td = new reactive.Variable(some(1.0), "T")
        val Ed = new reactive.Variable(some(9.0), "E")
        val Rd = c Then Td Else Ed
        val R1d = c Then Td Else Ed
        assert(Rd eq R1d)
        assert(R eq R1)
        def changeC(x : Option[Boolean], expected : Option[Double]) = (() => C :=! x, expected)
        def changeT(x : Option[Double], expected : Option[Double]) = (() => T :=! x, expected)
        def changeE(x : Option[Int], expected : Option[Double]) = (() => E :=! x, expected)

        ensureFunction(R, Some(9.0),
            changeC(Some(true), Some(1.0)),
            changeT(Some(3.5), Some(3.5)),
            changeT(Some(1.0), Some(1.0)),
            changeC(Some(false), Some(9.0)),
            changeE(Some(7), Some(7.0)))

        ensureFunction(Ri, Some(9.0),
            changeC(Some(true), Some(1.0)),
            changeT(Some(3.5), Some(3.5)),
            changeT(Some(1.0), Some(1.0)),
            changeC(Some(false), Some(9.0)))
    }
}
