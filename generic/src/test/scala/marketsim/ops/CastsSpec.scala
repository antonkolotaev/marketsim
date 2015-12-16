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

    val C = 2

    def optionInt[T](x : T)(implicit c : Conversion[T, Option[Int]]) = {
        assertResult(Some(C))(cast[Option[Int]](x))
    }

    def signalInt[T](x : T)(implicit c : Conversion[T, Signal[Int]]) = {
        val converted = cast[Signal[Int]](x)
        assertResult(C)(converted())
    }

    def signalOptionInt[T](x : T, changes : Int*)(implicit c : Conversion[T, Signal[Option[Int]]]) =
    {
        val converted = cast[Signal[Option[Int]]](x)
        val handler = mockFunction[Option[Int], Unit]("!")
        converted += handler

        assertResult(Some(C))(converted())
        changes foreach { ch => handler expects Some(ch) once () }
    }

    def functionInt[T](x : T)(implicit c : Conversion[T, () => Int]) = {
        val converted = cast[() => Int](x)
        assertResult(C)(converted())
    }

    def functionOptionInt[T](x : T)(implicit c : Conversion[T, () => Option[Int]]) = {
        val converted = cast[() => Option[Int]](x)
        assertResult(Some(C))(converted())
    }

    def unboundInt[T](x : T)(implicit c : Conversion[T, Unbound[Int]]) = {
        val converted = cast[Unbound[Int]](x)
        assertResult(C)(converted(ctx))
    }

    def unboundOptionInt[T](x : T)(implicit c : Conversion[T, Unbound[Option[Int]]]) = {
        val converted = cast[Unbound[Option[Int]]](x)
        assertResult(Some(C))(converted(ctx))
    }

    def unboundSignalInt[T](x : T)(implicit c : Conversion[T, Unbound[Signal[Int]]]) = {
        val converted = cast[Unbound[Signal[Int]]](x)
        assertResult(C)(converted(ctx)())
    }

    def unboundSignalOptionInt[T](x : T, changes : Int*)(implicit c : Conversion[T, Unbound[Signal[Option[Int]]]]) = {
        val converted = cast[Unbound[Signal[Option[Int]]]](x)
        val handler = mockFunction[Option[Int], Unit]("!")
        converted(ctx) += handler

        val bound = converted(ctx)

        assertResult(Some(C))(bound())

        changes foreach { ch => handler expects Some(ch) once () }
    }

    def unboundFunctionInt[T](x : T)(implicit c : Conversion[T, Unbound[() => Int]]) = {
        val converted = cast[Unbound[() => Int]](x)
        assertResult(C)(converted(ctx)())
    }

    def unboundFunctionOptionInt[T](x : T)(implicit c : Conversion[T, Unbound[() => Option[Int]]]) = {
        val converted = cast[Unbound[() => Option[Int]]](x)
        assertResult(Some(C))(converted(ctx)())
    }

    "A value of type T" should "cast to Option[T]"  in optionInt(C)
    it should "cast to Signal[T]"                   in signalInt(C)
    it should "cast to Signal[Option[T]]"           in signalOptionInt(C)
    it should "cast to Function[T]"                 in functionInt(C)
    it should "cast to Function[Option[T]]"         in functionOptionInt(C)
    it should "cast to Unbound[T]"                  in unboundInt(C)
    it should "cast to Unbound[Option[T]]"          in unboundOptionInt(C)
    it should "cast to Unbound[Signal[T]]"          in unboundSignalInt(C)
    it should "cast to Unbound[Signal[Option[T]]]"  in unboundSignalOptionInt(C)
    it should "cast to Unbound[() => T]"            in unboundFunctionInt(C)
    it should "cast to Unbound[() => Option[T]]"    in unboundFunctionOptionInt(C)

    val someC = Some(C)

    "A value of type Option[T]" should "cast to Signal[Option[T]]"  in signalOptionInt(someC)
    it should "cast to () => Option[T]"                             in functionOptionInt(someC)
    it should "cast to Unbound[Option[T]]"                          in unboundOptionInt(someC)
    it should "cast to Unbound[Signal[Option[T]]]"                  in unboundSignalOptionInt(someC)
    it should "cast to Unbound[() => Option[T]]"                    in unboundFunctionOptionInt(someC)

    def signalC = new Variable(2, "x")

    "A value of type Signal[T]" should "cast to Signal[Option[T]]" in {

        val original = signalC
        signalOptionInt(original, 3)
        original setAndCommit 3
    }

    it should "cast to Function[T]"                 in functionInt(signalC)
    it should "cast to Function[Option[T]]"         in functionOptionInt(signalC)
    it should "cast to Unbound[Signal[T]]"          in unboundSignalInt(signalC)
    it should "cast to Unbound[Signal[Option[T]]]"  in unboundSignalOptionInt(signalC)
    it should "cast to Unbound[() => T]"            in unboundFunctionInt(signalC)
    it should "cast to Unbound[() => Option[T]]"    in unboundFunctionOptionInt(signalC)

    val functionC = () => C

    "A value of type () => T" should "cast to () => Option[T]"  in functionOptionInt(functionC)
    it should "cast to Unbound[() => T]"                        in unboundFunctionInt(functionC)
    it should "cast to Unbound[() => Option[T]]"                in unboundFunctionOptionInt(functionC)

    val signalOptionC = new Variable(Some(2) : Option[Int], "x")

    "A value of type Signal[Option[T]]" should "cast to () => Option[T]" in functionOptionInt(signalOptionC)
    it should "cast to Unbound[Signal[Option[T]]]" in unboundSignalOptionInt(signalOptionC)
    it should "cast to Unbound[() => Option[T]]" in unboundFunctionOptionInt(signalOptionC)

    val functionOptionC = () => Some(C)

    "A value of type () => Option[T]" should "cast to Unbound[() => Option[T]]" in unboundFunctionOptionInt(functionOptionC)

    val unboundC = unbound(C)

    "A value of type Unbound[T]" should "cast to Unbound[Option[T]]" in unboundOptionInt(unboundC)
    it should "cast to Unbound[Signal[T]]" in unboundSignalInt(unboundC)
    it should "cast to Unbound[Signal[Option[T]]]" in unboundSignalOptionInt(unboundC)
    it should "cast to Unbound[() => T]" in unboundFunctionInt(unboundC)
    it should "cast to Unbound[() => Option[T]]" in unboundFunctionOptionInt(unboundC)

    val unboundOptionC = unbound(Some(2))

    "A value of type Unbound[Option[T]]" should "cast to Unbound[Signal[Option[T]]]" in unboundSignalOptionInt(unboundOptionC)
    it should "cast to Unbound[() => Option[T]]" in unboundFunctionOptionInt(unboundOptionC)

    "A value of type Unbound[Signal[T]]" should "cast to Unbound[Signal[Option[T]]]" in {

        val original = unbound(signalC)
        unboundSignalOptionInt(original, 3)

        original(ctx) setAndCommit 3
    }

    val unboundSignalC = unbound(new Variable(2, "x"))

    it should "cast to Unbound[() => T]" in unboundFunctionInt(unboundSignalC)
    it should "cast to Unbound[() => Option[T]]" in unboundSignalOptionInt(unboundSignalC)

    val unboundFunctionC = unbound(() => 2)

    "A value of type Unbound[() => T]" should "cast to Unbound[() => Option[T]]" in unboundFunctionOptionInt(unboundFunctionC)

}
