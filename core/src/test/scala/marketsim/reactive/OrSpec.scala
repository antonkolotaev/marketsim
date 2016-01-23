package marketsim
package reactive

class OrSpec extends TestBase {

    case class Id(a : Signal[Boolean]) extends UnaryBase(a, a(), "id")
    {
        val evaluated = new Event[Boolean]

        def F(x : Boolean) = {
            evaluated fire x
            x
        }
    }

    "b" should "be evaluated iff a is false" in {
        val a = new Variable(true, "a")
        val A = Id(a)
        val ah = mockFunction[Boolean, Unit]("A")
        A.evaluated += ah

        val b = new Variable(true, "b")
        val B = Id(b)
        val bh = mockFunction[Boolean, Unit]("B")
        B.evaluated += bh

        val R = Or(A,B)

        assertResult(R())(true)

        ah expects false
        a :=! false
        assertResult(R())(true)

        bh expects false
        b :=! false
        assertResult(R())(false)

        b :=! true
        ah expects true
        a :=! true
        assertResult(R())(true)
    }

}
