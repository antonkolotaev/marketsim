package marketsim

import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec

class ReferenceEqualitySpec extends FlatSpec with MockFactory with reactive.CleanMemo
{
    def assertMemoized(fs : (() => AnyRef)*) =
    {
        val evaluated = fs map { _ apply () }

        fs zip evaluated foreach {
            case (f, expected) =>
                val again = f()
                assert(again eq expected)
        }
    }

    "constants" should "be memoized" in {
        assertMemoized(() => Const(1), () => Const(2), () => Const(3.0), () => Const(3))
    }

    "unbound constants" should "be memoized" in {
        assertMemoized(() => unbound(Const(1)), () => unbound(Const(2)), () => unbound(Const(3.0)), () => unbound(Const(3)))
    }

    "unbound twice constants" should "be memoized" in {
        assertMemoized(
            () => unbound(unbound(Const(1))),
            () => unbound(unbound(Const(2))),
            () => unbound(unbound(Const(3.0))),
            () => unbound(unbound(Const(3))))
    }

    class Id[T] extends (T => T) {
        def apply(x : T) = x
    }

    val idInt2 = (x : Int) => x

    def idFunc(x : Int) = x
    val idInt3 = idFunc _

    object idInt extends Id[Int]
    object idDouble extends Id[Double]

    "function composition" should "be memoized" in {
        assertMemoized(
            () => Compose(Const(1), idInt2),
            () => Compose(Const(1.0), idDouble),
            () => Compose(Const(2), idInt),
            () => Compose(Const(3), idInt3))
    }

    "unbound function composition" should "be memoized" in {
        assertMemoized(
            () => unbound(Compose(Const(1), idInt2)),
            () => unbound(Compose(Const(1.0), idDouble)),
            () => unbound(Compose(Const(2), idInt)),
            () => unbound(Compose(Const(3), idInt3)))
    }
}
