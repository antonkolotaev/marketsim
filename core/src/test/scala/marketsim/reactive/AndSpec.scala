package marketsim
package reactive

class AndSpec extends TestBase {

    case class Id[T](a : Signal[T]) extends UnaryBase(a, a(), "id")
    {
        val evaluated = new Event[T]

        def F(x : T) = {
            evaluated fire x
            x
        }
    }

    "b" should "be evaluated iff a is true" in {
        val a = new Variable(true, "a")
        val A = Id(a)
        val ah = mockFunction[Boolean, Unit]("A")
        A.evaluated += ah

        val b = new Variable(true, "b")
        val B = Id(b)
        val bh = mockFunction[Boolean, Unit]("B")
        B.evaluated += bh

        val R = And(A,B)

        assertResult(R())(true)

        ah expects false
        a :=! false
        assertResult(R())(false)

        b :=! false
        assertResult(R())(false)

        b :=! true
        ah expects true
        a :=! true
        assertResult(R())(true)
    }
/*
    it should "be evaluated iff a is true (scalar AND option case)" in {
        val a = new Variable(true, "a")
        val A = Id(a)
        val ah = mockFunction[Boolean, Unit]("A")
        A.evaluated += ah

        val b = new Variable(some(true), "b")
        val B = Id(b)
        val bh = mockFunction[Option[Boolean], Unit]("B")
        B.evaluated += bh

        val R = And(A,B)

        assertResult(R())(Some(true))

        ah expects false
        a :=! false
        assertResult(R())(Some(false))

        b :=! Some(false)
        assertResult(R())(Some(false))

        b :=! Some(true)
        ah expects true
        a :=! true
        assertResult(R())(true)
    } */
}
