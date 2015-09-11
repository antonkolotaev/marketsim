package reactive

class UnarySpec extends TestBase
{
    class Chain
    {
        val v = variable("a")

        val f1 = toUpperCaseHandled(v.value, "a")
        val f2 = toUpperCaseHandled(f1.value, "A")

        assert(f2.value() == "A")
        assert(f1.value() == "A")
        assert(v.value() == "a")
    }

    "v <- a in a" should "make nothing" in new Chain {

        v.value setAndCommit "a"

        assert(f2.value() == "A")
        assert(f1.value() == "A")
        assert(v.value() == "a")
    }


    "v <- b in a" should "make f1 <- B and f2 <- B" in new Chain {

        /// ------------------------- v <- "b"

        v.handler expects "b" once()
        f1.handler expects "B" once()
        f2.handler expects "B" once()

        f1.back expects "b" once()
        f2.back expects "B" once()

        v.value setAndCommit "b"

        assert(f2.value() == "B")
        assert(f1.value() == "B")
        assert(v.value() == "b")

    }

    "f1.dispose" should "release f1 and f2" in new Chain {

        f1.value dispose()
        v.handler expects "c" once()
        v.value setAndCommit "c"

        assert(!v.value.disposed)
        assert(f1.value.disposed)
        assert(f2.value.disposed)

    }

    "v <- A in a" should "cause only f1 re-evaluation" in new Chain {

        /// ------------------------- v <- "B"

        v.handler expects "A" once()

        f1.back expects "A" once ()

        v.value setAndCommit "A"

        assert(f2.value() == "A")
        assert(f1.value() == "A")
        assert(v.value() == "A")
    }

}
