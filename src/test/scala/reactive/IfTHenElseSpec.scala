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

        b.value set "z"
        b.value set "z"
    }

    "a <- x" should "update the result to X" in new Fork {

        a.handler expects "x" once ()

        A.back expects "x" once ()

        fork.handler expects "X" once ()

        a.value set "x"

        C.back expects "X" once ()

        assert(C.value() == "X")
    }

    "condition <- false" should "change the result to B" in new Fork {

        condition.handler expects false once()
        fork.handler expects "B" once()

        condition.value set false

        C.back expects "B" once ()
        assert(C.value() == "B")
    }

}
