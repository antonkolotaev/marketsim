package marketsim
package ops

class OpsSpec extends EnsureChanges {

    "_.isSome" should "be an unary boolean signal" in {
        {
            val v = new reactive.Variable(Some(1) : Option[Int], "v")
            def change(x : Option[Int], expected : Boolean) = (() => v :=! x, expected)
            val s = v.isSome
            val s2 = v.isSome
            assert(s eq s2)
            ensureSignal(s, true, change(None, false), change(Some(3), true))
        }
        {
            val v = new reactive.Variable(Some(1.0) : Option[Double], "v")
            def change(x : Option[Double], expected : Boolean) = (() => v :=! x, expected)
            val s = v.isSome
            val s2 = v.isSome
            assert(s eq s2)
            ensureSignal(s, true, change(None, false), change(Some(3.0), true))
        }
    }

    "_.getSome" should "be an unary signal T" in {
        {
            val v = new reactive.Variable(Some(1) : Option[Int], "v")
            def change(x : Option[Int], expected : Int) = (() => v :=! x, expected)
            val s = v.getSome
            val s2 = v.getSome
            assert(s eq s2)
            ensureSignal(s, 1, change(Some(2), 2), change(Some(3), 3))
        }
        {
            val v = new reactive.Variable(Some(1.0) : Option[Double], "v")
            def change(x : Option[Double], expected : Double) = (() => v :=! x, expected)
            val s = v.getSome
            val s2 = v.getSome
            assert(s eq s2)
            ensureSignal(s, 1.0, change(Some(2.0), 2.0), change(Some(3.0), 3.0))
        }
    }
}
