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

    def ensureSignalOptionInt(converted : Signal[Option[Int]], changes : (() => Unit, Int)*) : Unit =
    {
        val handler = mockFunction[Option[Int], Unit]("!")
        converted += handler

        assertResult(Some(C))(converted())
        changes foreach { case (action, ch) => {
            handler expects Some(ch) once ()
            action()
            val actual = converted()
            assertResult(Some(ch))(actual)
        } }
    }

    def ensureSignalInt(converted : Signal[Int], changes : (() => Unit, Int)*) : Unit =
    {
        val handler = mockFunction[Int, Unit]("!")
        converted += handler

        assertResult(C)(converted())
        changes foreach { case (action, ch) => {
            handler expects ch once ()
            action()
            assertResult(ch)(converted())
        } }
    }

    def ensureFunctionOptionInt(converted : () => Option[Int], changes : (() => Unit, Int)*) : Unit =
    {
        assertResult(Some(C))(converted())
        changes foreach { case (action, expected) =>
            action()
            val actual = converted()
            assertResult(Some(expected))(actual)
        }
    }

    def ensureFunctionInt(converted : () => Int, changes : (() => Unit, Int)*) : Unit =
    {
        assertResult(C)(converted())
        changes foreach { case (action, ch) =>
            action()
            assertResult(ch)(converted())
        }
    }

    def signalOptionInt[T](x : T, changes : (() => Unit, Int)*)(implicit c : Conversion[T, Signal[Option[Int]]]) =
    {
        val converted = cast[Signal[Option[Int]]](x)
        ensureSignalOptionInt(converted, changes : _*)
    }

    def functionInt[T](x : T, changes : (() => Unit, Int)*)(implicit c : Conversion[T, () => Int]) = {
        val converted = cast[() => Int](x)
        ensureFunctionInt(converted, changes : _*)
    }

    def functionOptionInt[T](x : T, changes : (() => Unit, Int)*)(implicit c : Conversion[T, () => Option[Int]]) = {
        val converted = cast[() => Option[Int]](x)
        ensureFunctionOptionInt(converted, changes : _*)
    }

    def unboundInt[T](x : T)(implicit c : Conversion[T, Unbound[Int]]) = {
        val converted = cast[Unbound[Int]](x)
        assertResult(C)(converted(ctx))
    }

    def unboundOptionInt[T](x : T)(implicit c : Conversion[T, Unbound[Option[Int]]]) = {
        val converted = cast[Unbound[Option[Int]]](x)
        assertResult(Some(C))(converted(ctx))
    }

    def unboundSignalInt[T](x : T, changes : (() => Unit, Int)*)(implicit c : Conversion[T, Unbound[Signal[Int]]]) = {
        val converted = cast[Unbound[Signal[Int]]](x)
        val bound = converted(ctx)

        ensureSignalInt(bound, changes : _*)
    }

    def unboundSignalOptionInt[T](x : T, changes : (() => Unit, Int)*)
                                 (implicit c : Conversion[T, Unbound[Signal[Option[Int]]]]) = {
        val converted = cast[Unbound[Signal[Option[Int]]]](x)
        val bound = converted(ctx)

        ensureSignalOptionInt(bound, changes : _*)
    }

    def unboundFunctionInt[T](x : T, changes : (() => Unit, Int)*)
                             (implicit c : Conversion[T, Unbound[() => Int]]) = {
        val converted = cast[Unbound[() => Int]](x)
        val bound = converted(ctx)

        ensureFunctionInt(bound, changes : _*)
    }

    def unboundFunctionOptionInt[T](x : T, changes : (() => Unit, Int)*)
                                   (implicit c : Conversion[T, Unbound[() => Option[Int]]]) = {
        val converted = cast[Unbound[() => Option[Int]]](x)
        val bound = converted(ctx)

        ensureFunctionOptionInt(bound, changes : _*)
    }

    class A_FunctionInt
    {
        var V = C
        val A = () => V
        val someA = () => Some(V)
        val unboundA = unbound(A)
        def changeA(x : Int) = (() => V = x, x)
    }

    class A_SignalInt
    {
        val A = new Variable(C, "x")
        val someA = cast[Signal[Option[Int]]](A)
        val unboundA = unbound(A)

        someA += { x => println(x) }

        def changeA(x : Int) = (() => A :=! x, x)
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

    "A value of type Signal[T]" should "cast to Signal[Option[T]]" in new A_SignalInt { signalOptionInt(A, changeA(3)) }

    it should "cast to Function[T]"                 in new A_SignalInt { functionInt(A, changeA(4)) }
    it should "cast to Function[Option[T]]"         in new A_SignalInt { functionOptionInt(A, changeA(5)) }
    it should "cast to Unbound[Signal[T]]"          in new A_SignalInt { unboundSignalInt(A, changeA(6)) }
    it should "cast to Unbound[Signal[Option[T]]]"  in new A_SignalInt {
        unboundSignalOptionInt(A, changeA(7))
    }
    it should "cast to Unbound[() => T]"            in new A_SignalInt { unboundFunctionInt(A, changeA(8)) }
    it should "cast to Unbound[() => Option[T]]"    in new A_SignalInt { unboundFunctionOptionInt(A, changeA(9)) }

    "A value of type () => T" should "cast to () => Option[T]"  in new A_FunctionInt { functionOptionInt(A, changeA(10)) }
    it should "cast to Unbound[() => T]"                        in new A_FunctionInt { unboundFunctionInt(A, changeA(11)) }
    it should "cast to Unbound[() => Option[T]]"                in new A_FunctionInt { unboundFunctionOptionInt(A, changeA(12)) }

    "A value of type Signal[Option[T]]" should "cast to () => Option[T]" in new A_SignalInt {//
        functionOptionInt(someA, changeA(13))
    }

    it should "cast to Unbound[Signal[Option[T]]]" in new A_SignalInt { unboundSignalOptionInt(someA, changeA(14)) }
    it should "cast to Unbound[() => Option[T]]" in new A_SignalInt { unboundFunctionOptionInt(someA, changeA(15)) }

    "A value of type () => Option[T]" should "cast to Unbound[() => Option[T]]" in new A_FunctionInt {
        unboundFunctionOptionInt(someA, changeA(16))
    }

    val unboundC = unbound(C)

    "A value of type Unbound[T]" should "cast to Unbound[Option[T]]" in unboundOptionInt(unboundC)
    it should "cast to Unbound[Signal[T]]" in unboundSignalInt(unboundC)
    it should "cast to Unbound[Signal[Option[T]]]" in unboundSignalOptionInt(unboundC)
    it should "cast to Unbound[() => T]" in unboundFunctionInt(unboundC)
    it should "cast to Unbound[() => Option[T]]" in unboundFunctionOptionInt(unboundC)

    val unboundOptionC = unbound(Some(2))

    "A value of type Unbound[Option[T]]" should "cast to Unbound[Signal[Option[T]]]" in unboundSignalOptionInt(unboundOptionC)
    it should "cast to Unbound[() => Option[T]]" in unboundFunctionOptionInt(unboundOptionC)

    "A value of type Unbound[Signal[T]]" should "cast to Unbound[Signal[Option[T]]]" in new A_SignalInt {
        unboundSignalOptionInt(unbound(A), changeA(17))
    }

    it should "cast to Unbound[() => T]" in new A_SignalInt { unboundFunctionInt(unboundA, changeA(18)) }
    it should "cast to Unbound[() => Option[T]]" in new A_SignalInt { unboundSignalOptionInt(unboundA, changeA(19)) }

    "A value of type Unbound[() => T]" should "cast to Unbound[() => Option[T]]" in new A_FunctionInt {
        unboundFunctionOptionInt(unboundA, changeA(20))
    }

}
