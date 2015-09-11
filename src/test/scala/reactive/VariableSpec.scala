package reactive

class VariableSpec extends TestBase {

    "A variable" should "change its value" in {
        val v = variable("A")

        v.handler expects "B" once()
        v.value setAndCommit "B"

        assert(v.value() == "B")
    }

    it should "be unchanged if untouched" in {
        val v = variable("A")
        v.handler expects * never()

        assert(v.value() == "A")

        v.value setAndCommit "A"

        assert(v.value() == "A")
    }
}
