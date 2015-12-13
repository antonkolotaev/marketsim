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

        val original = Some(2)
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

}
