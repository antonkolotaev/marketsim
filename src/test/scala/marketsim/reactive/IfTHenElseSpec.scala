package marketsim
package reactive

class IfTHenElseSpec extends TestBase {

    class Fork
    {
        val condition = variable(true)

        val a = variable("a")
        val b = variable("b")

        val A = toUpperCase(a.value, "A")
        val B = toUpperCase(b.value, "B")

        val fork = ifThenElse(condition.value, A.value, B.value)

        val C = toUpperCase(fork.value, "A")
    }

    "b <- z" should "update only b" in new Fork {

        b.handler expects "z" once ()

        b.value setAndCommit "z"
        b.value setAndCommit "z"
    }

    "a <- x" should "update the result to X" in new Fork {

        a.handler expects "x" once ()

        A.back expects "x" once ()

        fork.handler expects "X" once ()

        a.value setAndCommit "x"

        C.back expects "X" once ()

        assert(C.value() == "X")
    }

    "A.dispose" should "leave only a, b and B" in new Fork {

        A.value dispose ()

        a.handler expects "z" once()

        a.value setAndCommit "z"

        assert(!a.value.disposed)
        assert(!b.value.disposed)

        assert(A.value.disposed)
        assert(!B.value.disposed)

        assert(C.value.disposed)
    }

    "condition <- false" should "change the result to B" in new Fork {

        condition.handler expects false once()
        fork.handler expects "B" once()

        condition.value setAndCommit false

        C.back expects "B" once ()
        assert(C.value() == "B")
    }

}
