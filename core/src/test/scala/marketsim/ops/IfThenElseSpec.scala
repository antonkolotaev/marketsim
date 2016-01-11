package marketsim
package ops

class IfThenElseSpec extends EnsureChanges {

    import ops.Implicits._
    import conversions.Implicits._

    class Options
    {
        var C = some(false)
        var T = some(1)
        var E = some(9)

        val Td = some(1.0)
        val Ed = some(9.0)

        def check(R : () => Option[Int]): Unit =
        {
            assertResult(R())(Some(9))

            E = Some(7)
            assertResult(R())(Some(7))

            C = Some(true)
            assertResult(R())(Some(1))

            C = None
            assertResult(R())(None)

            C = Some(true)
            T = Some(3)
            assertResult(R())(Some(3))

            E = None
            assertResult(R())(Some(3))

            C = Some(false)
            assertResult(R())(None)
        }
    }

    "IfThenElse of options" should "be an option" in new Options {
        def R = C Then T Else E
        val R1 = C Then T Else E
        // checking that Option[Double] doesn't interfere with Option[Int]
        val Rd = C Then Td Else Ed
        val R1d = C Then Td Else Ed
        //assert(Rd eq R1d)
        //assert(R eq R1)
        check(() => R)
    }

    "IfThenElse of unbound options" should "be an unbound option" in new Options {
        def R = unbound(C) Then unbound(T) Else unbound(E)
        val R1 = unbound(C) Then unbound(T) Else unbound(E)
        // checking that Option[Double] doesn't interfere with Option[Int]
        val Rd = unbound(C) Then unbound(Td) Else unbound(Ed)
        val R1d = unbound(C) Then unbound(Td) Else unbound(Ed)
        assert(Rd eq R1d)
        assert(R eq R1)
        check(() => R(ctx))
    }

    class Variables
    {
        val C = new reactive.Variable(false, "C")
        val T = new reactive.Variable(1.0, "T")
        val E = new reactive.Variable(9, "E")

        def changeC(x : Boolean, expected : Double) = (() => C :=! x, expected)
        def changeT(x : Double, expected : Double) = (() => T :=! x, expected)
        def changeE(x : Int, expected : Double) = (() => E :=! x, expected)

        val Td = new reactive.Variable(1.0, "T")
        val Ed = new reactive.Variable(9.0, "E")

        def ensure(R : reactive.Signal[Double]) =
            ensureSignal(R, 9.0, changeC(true, 1.0), changeT(3.5, 3.5), changeC(false, 9.0), changeE(7, 7.0))
    }

    "IfThenElse of signals" should "be a signal" in new Variables {
        val R = C Then T Else E
        val R1 = C Then T Else E
        // checking that Signal[Double] doesn't interfere with Signal[Int]
        val Rd = C Then Td Else Ed
        val R1d = C Then Td Else Ed
        assert(Rd eq R1d)
        assert(R eq R1)

        ensure(R)
    }

    "IfThenElse of unbound signals" should "be an unbound signal" in new Variables {
        val R = unbound(C) Then unbound(T) Else unbound(E)
        val R1 = unbound(C) Then unbound(T) Else unbound(E)
        // checking that Signal[Double] doesn't interfere with Signal[Int]
        val Rd = unbound(C) Then unbound(Td) Else unbound(Ed)
        val R1d = unbound(C) Then unbound(Td) Else unbound(Ed)
        assert(Rd eq R1d)
        assert(R eq R1)

        ensure(R(ctx))
    }

    class OptionVariables
    {
        val C = new reactive.Variable(some(false), "C")
        val T = new reactive.Variable(some(1.0), "T")
        val E = new reactive.Variable(some(9), "E")

        def changeC(x : Option[Boolean], expected : Option[Double]) = (() => C :=! x, expected)
        def changeT(x : Option[Double], expected : Option[Double]) = (() => T :=! x, expected)
        def changeE(x : Option[Int], expected : Option[Double]) = (() => E :=! x, expected)

        def ensure(R : reactive.Signal[Option[Double]], Ri : reactive.Signal[Option[Double]]) = {
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
    }

    "IfThenElse of option signals" should "be an option signal" in new OptionVariables {
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
        ensure(R, Ri)
    }

    "IfThenElse of unbound option signals" should "be an unbound option signal" in new OptionVariables {
        val R = unbound(C) Then unbound(T) Else E
        val R1 = unbound(C) Then unbound(T) Else E
        val Ri = unbound(C) Then unbound(T) Else 9
        // checking that Signal[Option[Double]] doesn't interfere with Signal[Option[Int]]
        val Td = new reactive.Variable(some(1.0), "T")
        val Ed = new reactive.Variable(some(9.0), "E")
        val Rd = unbound(C) Then unbound(Td) Else Ed
        val R1d = unbound(C) Then unbound(Td) Else Ed
        assert(Rd eq R1d)
        assert(R eq R1)
        ensure(R(ctx), Ri(ctx))
    }

    class Functions
    {
        val c = new reactive.Variable(false, "C")
        val C = c : () => Boolean
        val T = new reactive.Variable(1.0, "T")
        val E = new reactive.Variable(9, "E")
        val Td = new reactive.Variable(1.0, "T")
        val Ed = new reactive.Variable(9.0, "E")
        val Ei = 12
        def changeC(x : Boolean, expected : Double) = (() => c :=! x, expected)
        def changeT(x : Double, expected : Double) = (() => T :=! x, expected)
        def changeE(x : Int, expected : Double) = (() => E :=! x, expected)

        def ensure(R : () => Double, Ri : () => Double) =
        {
            ensureFunction(R, 9.0,
                changeC(true, 1.0),
                changeT(3.5, 3.5),
                changeT(1.0, 1.0),
                changeC(false, 9.0),
                changeE(7, 7.0),
                changeE(9, 9.0))

            ensureFunction(Ri, 12.0,
                changeC(true, 1.0),
                changeT(3.5, 3.5),
                changeT(1.0, 1.0),
                changeC(false, 12.0))
        }
    }

    "IfThenElse of functions" should "be a function" in new Functions {
        val R = C Then T Else E
        val Ri = C Then T Else Ei
        val R1 = C Then T Else E
        // checking that () => Double doesn't interfere with () => Int
        val Rd = C Then Td Else Ed
        val R1d = C Then Td Else Ed
        assert(Rd eq R1d)
        assert(R eq R1)

        ensure(R, Ri)
    }

    "IfThenElse of unbound functions" should "be an unbound function" in new Functions {
        val R = unbound(C) Then unbound(T) Else E
        val Ri = unbound(C) Then unbound(T) Else Ei
        val R1 = unbound(C) Then unbound(T) Else E
        // checking that () => Double doesn't interfere with () => Int
        val Rd = unbound(C) Then unbound(Td) Else Ed
        val R1d = unbound(C) Then unbound(Td) Else Ed
        assert(Rd eq R1d)
        assert(R eq R1)

        ensure(R(ctx), Ri(ctx))
    }
    
    class OptionFunctions 
    {
        val c = new reactive.Variable(some(false), "C")
        val C = c : () => Option[Boolean]
        val T = new reactive.Variable(some(1.0), "T")
        val E = new reactive.Variable(some(9), "E")
        val Ei = 9
        val Td = new reactive.Variable(some(1.0), "T")
        val Ed = new reactive.Variable(some(9.0), "E")

        def changeC(x : Option[Boolean], expected : Option[Double]) = (() => c :=! x, expected)
        def changeT(x : Option[Double], expected : Option[Double]) = (() => T :=! x, expected)
        def changeE(x : Option[Int], expected : Option[Double]) = (() => E :=! x, expected)

        def ensure(R : () => Option[Double], Ri : () => Option[Double]) =
        {
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

    "IfThenElse of option functions" should "be an option function" in new OptionFunctions {
        val R = C Then T Else E
        val Ri = C Then T Else Ei
        val R1 = C Then T Else E
        // checking that () => Option[Double] doesn't interfere with () => Option[Int]
        val Rd = C Then Td Else Ed
        val R1d = C Then Td Else Ed
        assert(Rd eq R1d)
        assert(R eq R1)
        ensure(R, Ri)
    }

    "IfThenElse of unbound option functions" should "be an unbound option function" in new OptionFunctions {
        val R = unbound(C) Then unbound(T) Else E
        val Ri = unbound(C) Then unbound(T) Else Ei
        val R1 = unbound(C) Then unbound(T) Else E
        // checking that () => Option[Double] doesn't interfere with () => Option[Int]
        val Rd = unbound(C) Then unbound(Td) Else Ed
        val R1d = unbound(C) Then unbound(Td) Else Ed
        assert(Rd eq R1d)
        assert(R eq R1)
        ensure(R(ctx), Ri(ctx))
    }
}
