package reactive

class BinarySpec extends TestBase {

    trait ABBC {
        val a = variable("a")
        val b = variable("b")
        val c = variable("c")

        val ab = concat(a.value, b.value, "a", "b")
        val bc = concat(b.value, c.value, "b", "c")
        val abbc = concat(ab.value, bc.value, "(AB)", "(BC)")

        assert(abbc.value() == "((AB)(BC))")
        assert(ab.value() == "(AB)")
        assert(bc.value() == "(BC)")

    }

    "a <- x in (AB)(BC)" should "give (XB)(BC)" in new ABBC {

        a.handler expects "x" once()
        ab.handler expects "(XB)" once()
        abbc.handler expects "((XB)(BC))" once()

        ab.back expects ("x", "b") once ()
        abbc.back expects ("(XB)", "(BC)") once ()

        a.value set "x"

        assert(abbc.value() == "((XB)(BC))")
        assert(ab.value() == "(XB)")
        assert(bc.value() == "(BC)")
    }

    "b <- y in (AB)(BC)" should "give (AY)(YC)" in new ABBC {

        b.handler expects "y" once()
        ab.handler expects "(AY)" once()
        bc.handler expects "(YC)" once()
        abbc.handler expects "((AY)(YC))" once()

        ab.back expects ("a", "y") once ()
        bc.back expects ("y", "c") once ()
        abbc.back expects ("(AY)", "(YC)") once ()

        b.value set "y"

        assert(abbc.value() == "((AY)(YC))")
        assert(ab.value() == "(AY)")
        assert(bc.value() == "(YC)")

        ab.value dispose ()
        b.handler expects "z" once()
        bc.back expects ("z", "c") once ()
        bc.handler expects "(ZC)" once()

        b.value set "z"
    }

}
