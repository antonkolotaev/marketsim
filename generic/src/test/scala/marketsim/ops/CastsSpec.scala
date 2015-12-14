package marketsim
package ops

import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec

class CastsSpec extends FlatSpec with MockFactory {

    import Casts._
    import reactive._

    val ctx = new Context {}

    def cast[To] = new {
        def apply[From](x : From)(implicit c : Conversion[From, To]) : To = c convert x
    }

    "A value of type T" should "cast to Option[T]" in {

        assertResult(Some(2))(cast[Option[Int]](2))

    }

    it should "cast to Signal[T]" in {

        val original = 2
        val converted = cast[Signal[Int]](original)
        assertResult(2)(converted())

    }

    it should "cast to Signal[Option[T]]" in {

        val original = 2
        val converted = cast[Signal[Option[Int]]](original)
        assertResult(Some(2))(converted())

    }

    it should "cast to Function[T]" in {

        val original = 2
        val converted = cast[() => Int](original)
        assertResult(2)(converted())

    }

    it should "cast to Function[Option[T]]" in {

        val original = 2
        val converted = cast[() => Option[Int]](original)
        assertResult(Some(2))(converted())

    }

    it should "cast to Unbound[T]" in {

        val original = 2
        val converted = cast[Unbound[Int]](original)
        assertResult(2)(converted(ctx))

    }
    it should "cast to Unbound[Option[T]]" in {

        val original = 2
        val converted = cast[Unbound[Option[Int]]](original)
        assertResult(Some(2))(converted(ctx))

    }

    it should "cast to Unbound[Signal[T]]" in {

        val original = 2
        val converted = cast[Unbound[Signal[Int]]](original)
        assertResult(2)(converted(ctx)())

    }

    it should "cast to Unbound[Signal[Option[T]]]" in {

        val original = 2
        val converted = cast[Unbound[Signal[Option[Int]]]](original)
        assertResult(Some(2))(converted(ctx)())

    }

    it should "cast to Unbound[() => T]" in {

        val original = 2
        val converted = cast[Unbound[() => Int]](original)
        assertResult(2)(converted(ctx)())

    }

    it should "cast to Unbound[() => Option[T]]" in {

        val original = 2
        val converted = cast[Unbound[() => Option[Int]]](original)
        assertResult(Some(2))(converted(ctx)())

    }

    "A value of type Option[T]" should "cast to Signal[Option[T]]" in {

        val original = Some(2) : Option[Int]
        val converted = cast[Signal[Option[Int]]](original)
        assertResult(Some(2))(converted())

    }

    it should "cast to Function[Option[T]]" in {

        val original = Some(2)
        val converted = cast[() => Option[Int]](original)
        assertResult(Some(2))(converted())

    }

    it should "cast to Unbound[Option[T]]" in {

        val original = Some(2)
        val converted = cast[Unbound[Option[Int]]](original)
        assertResult(Some(2))(converted(ctx))

    }

    it should "cast to Unbound[Signal[Option[T]]]" in {

        val original = Some(2)
        val converted = cast[Unbound[Signal[Option[Int]]]](original)
        assertResult(Some(2))(converted(ctx)())

    }

    it should "cast to Unbound[() => Option[T]]" in {

        val original = Some(2)
        val converted = cast[Unbound[() => Option[Int]]](original)
        assertResult(Some(2))(converted(ctx)())

    }

    "A value of type Signal[T]" should "cast to Signal[Option[T]]" in {

        val original = new Variable(2, "x")
        val converted = cast[Signal[Option[Int]]](original)
        val handler = mockFunction[Option[Int], Unit]("!")
        converted += handler

        assertResult(Some(2))(converted())
        handler expects Some(3) once ()

        original setAndCommit 3
    }

    it should "cast to Function[T]" in {

        val original = new Variable(2, "x")
        val converted = cast[() => Int](original)
        assertResult(2)(converted())

    }

    it should "cast to Function[Option[T]]" in {

        val original = new Variable(2, "x")
        val converted = cast[() => Option[Int]](original)
        assertResult(Some(2))(converted())

    }

    it should "cast to Unbound[Signal[T]]" in {

        val original = new Variable(2, "x")
        val converted = cast[Unbound[Signal[Int]]](original)
        assertResult(2)(converted(ctx)())

    }

    it should "cast to Unbound[Signal[Option[T]]]" in {

        val original = new Variable(2, "x")
        val converted = cast[Unbound[Signal[Option[Int]]]](original)
        assertResult(Some(2))(converted(ctx)())

    }

    it should "cast to Unbound[() => T]" in {

        val original = new Variable(2, "x")
        val converted = cast[Unbound[() => Int]](original)
        assertResult(2)(converted(ctx)())

    }

    it should "cast to Unbound[() => Option[T]]" in {

        val original = new Variable(2, "x")
        val converted = cast[Unbound[() => Option[Int]]](original)
        assertResult(Some(2))(converted(ctx)())

    }

    "A value of type Function[T]" should "cast to Function[Option[T]]" in {

        val original = () => 2
        val converted = cast[() => Option[Int]](original)
        assertResult(Some(2))(converted())

    }

    it should "cast to Unbound[() => T]" in {

        val original = () => 2
        val converted = cast[Unbound[() => Int]](original)
        assertResult(2)(converted(ctx)())

    }

    it should "cast to Unbound[() => Option[T]]" in {

        val original = () => 2
        val converted = cast[Unbound[() => Option[Int]]](original)
        assertResult(Some(2))(converted(ctx)())

    }

    "A value of type Signal[Option[T]]" should "cast to Function[Option[T]]" in {

        val original = new Variable(Some(2) : Option[Int], "x")
        val converted = cast[() => Option[Int]](original)
        assertResult(Some(2))(converted())

    }

    it should "cast to Unbound[Signal[Option[T]]]" in {

        val original = new Variable(Some(2) : Option[Int], "x")
        val converted = cast[Unbound[Signal[Option[Int]]]](original)
        assertResult(Some(2))(converted(ctx)())

    }

    it should "cast to Unbound[Function[Option[T]]]" in {

        val original = new Variable(Some(2) : Option[Int], "x")
        val converted = cast[Unbound[() => Option[Int]]](original)
        assertResult(Some(2))(converted(ctx)())

    }

    "A value of type Function[Option[T]]" should "cast to Unbound[Function[Option[T]]]" in {

        val original = () => Some(2)
        val converted = cast[Unbound[() => Option[Int]]](original)
        assertResult(Some(2))(converted(ctx)())

    }

    "A value of type Unbound[T]" should "cast to Unbound[Option[T]]" in {

        val original = unbound(2)
        val converted = cast[Unbound[Option[Int]]](original)
        assertResult(Some(2))(converted(ctx))

    }

    it should "cast to Unbound[Signal[T]]" in {
        val original = unbound(2)
        val converted = cast[Unbound[Signal[Int]]](original)
        assertResult(2)(converted(ctx)())
    }

    it should "cast to Unbound[Signal[Option[T]]]" in {

        val original = unbound(2)
        val converted = cast[Unbound[Signal[Option[Int]]]](original)
        assertResult(Some(2))(converted(ctx)())

    }

    it should "cast to Unbound[() => T]" in {

        val original = unbound(2)
        val converted = cast[Unbound[() => Int]](original)
        assertResult(2)(converted(ctx)())

    }

    it should "cast to Unbound[() => Option[T]]" in {

        val original = unbound(2)
        val converted = cast[Unbound[() => Option[Int]]](original)
        assertResult(Some(2))(converted(ctx)())

    }

    "A value of type Unbound[Option[T]]" should "cast to Unbound[Signal[Option[T]]]" in {

        val original = unbound(Some(2))
        val converted = cast[Unbound[Signal[Option[Int]]]](original)
        assertResult(Some(2))(converted(ctx)())

    }

    it should "cast to Unbound[() => Option[T]]" in {

        val original = unbound(Some(2))
        val converted = cast[Unbound[() => Option[Int]]](original)
        assertResult(Some(2))(converted(ctx)())

    }

    "A value of type Unbound[Signal[T]]" should "cast to Unbound[Signal[Option[T]]]" in {

        val raw = new Variable(2, "x")
        val original = unbound(raw)
        val converted = cast[Unbound[Signal[Option[Int]]]](original)
        val handler = mockFunction[Option[Int], Unit]("!")
        converted(ctx) += handler

        assertResult(Some(2))(converted(ctx)())
        handler expects Some(3) once ()

        raw setAndCommit 3
    }

    it should "cast to Unbound[() => T]" in {

        val original = unbound(new Variable(2, "x"))
        val converted = cast[Unbound[() => Int]](original)
        assertResult(2)(converted(ctx)())

    }

    it should "cast to Unbound[() => Option[T]]" in {

        val original = unbound(new Variable(2, "x"))
        val converted = cast[Unbound[() => Option[Int]]](original)
        assertResult(Some(2))(converted(ctx)())

    }

    "A value of type Unbound[Function[T]]" should "cast to Unbound[Function[Option[T]]]" in {

        val original = unbound(() => 2)
        val converted = cast[Unbound[() => Option[Int]]](original)
        assertResult(Some(2))(converted(ctx)())

    }

}
