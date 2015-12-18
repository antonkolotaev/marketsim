package marketsim
package ops

class CastsSpec extends EnsureChanges {

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

    def signalOptionInt[T](x : T, initial : Int, changes : (() => Unit, Int)*)
                          (implicit c : Conversion[T, Signal[Option[Int]]]) =
    {
        val converted = cast[Signal[Option[Int]]](x)
        ensureSignalOptionInt(converted, initial, changes : _*)
    }

    def functionInt[T](x : T, initial : Int, changes : (() => Unit, Int)*)
                      (implicit c : Conversion[T, () => Int]) = {
        val converted = cast[() => Int](x)
        ensureFunctionInt(converted, initial, changes : _*)
    }

    def functionOptionInt[T](x : T, initial : Int, changes : (() => Unit, Int)*)
                            (implicit c : Conversion[T, () => Option[Int]]) = {
        val converted = cast[() => Option[Int]](x)
        ensureFunctionOptionInt(converted, initial, changes : _*)
    }

    def unboundInt[T](x : T)(implicit c : Conversion[T, Unbound[Int]]) = {
        val converted = cast[Unbound[Int]](x)
        assertResult(C)(converted(ctx))
    }

    def unboundOptionInt[T](x : T)(implicit c : Conversion[T, Unbound[Option[Int]]]) = {
        val converted = cast[Unbound[Option[Int]]](x)
        assertResult(Some(C))(converted(ctx))
    }

    def unboundSignalInt[T](x : T, initial : Int, changes : (() => Unit, Int)*)
                           (implicit c : Conversion[T, Unbound[Signal[Int]]]) = {
        val converted = cast[Unbound[Signal[Int]]](x)
        val bound = converted(ctx)

        ensureSignalInt(bound, initial, changes : _*)
    }

    def unboundSignalOptionInt[T](x : T, initial : Int, changes : (() => Unit, Int)*)
                                 (implicit c : Conversion[T, Unbound[Signal[Option[Int]]]]) = {
        val converted = cast[Unbound[Signal[Option[Int]]]](x)
        val bound = converted(ctx)

        ensureSignalOptionInt(bound, initial, changes : _*)
    }

    def unboundFunctionInt[T](x : T, initial : Int, changes : (() => Unit, Int)*)
                             (implicit c : Conversion[T, Unbound[() => Int]]) = {
        val converted = cast[Unbound[() => Int]](x)
        val bound = converted(ctx)

        ensureFunctionInt(bound, initial, changes : _*)
    }

    def unboundFunctionOptionInt[T](x : T, initial : Int, changes : (() => Unit, Int)*)
                                   (implicit c : Conversion[T, Unbound[() => Option[Int]]]) = {
        val converted = cast[Unbound[() => Option[Int]]](x)
        val bound = converted(ctx)

        ensureFunctionOptionInt(bound, initial, changes : _*)
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
    it should "cast to Signal[Option[T]]"           in signalOptionInt(C,C)
    it should "cast to Function[T]"                 in functionInt(C,C)
    it should "cast to Function[Option[T]]"         in functionOptionInt(C,C)
    it should "cast to Unbound[T]"                  in unboundInt(C)
    it should "cast to Unbound[Option[T]]"          in unboundOptionInt(C)
    it should "cast to Unbound[Signal[T]]"          in unboundSignalInt(C,C)
    it should "cast to Unbound[Signal[Option[T]]]"  in unboundSignalOptionInt(C,C)
    it should "cast to Unbound[() => T]"            in unboundFunctionInt(C,C)
    it should "cast to Unbound[() => Option[T]]"    in unboundFunctionOptionInt(C,C)

    val someC = Some(C)

    "A value of type Option[T]" should "cast to Signal[Option[T]]"  in signalOptionInt(someC,C)
    it should "cast to () => Option[T]"                             in functionOptionInt(someC,C)
    it should "cast to Unbound[Option[T]]"                          in unboundOptionInt(someC)
    it should "cast to Unbound[Signal[Option[T]]]"                  in unboundSignalOptionInt(someC,C)
    it should "cast to Unbound[() => Option[T]]"                    in unboundFunctionOptionInt(someC,C)

    "A value of type Signal[T]" should "cast to Signal[Option[T]]" in new A_SignalInt { signalOptionInt(A,C, changeA(3)) }

    it should "cast to Function[T]"                 in new A_SignalInt { functionInt(A,C, changeA(4)) }
    it should "cast to Function[Option[T]]"         in new A_SignalInt { functionOptionInt(A,C, changeA(5)) }
    it should "cast to Unbound[Signal[T]]"          in new A_SignalInt { unboundSignalInt(A,C, changeA(6)) }
    it should "cast to Unbound[Signal[Option[T]]]"  in new A_SignalInt {
        unboundSignalOptionInt(A,C, changeA(7))
    }
    it should "cast to Unbound[() => T]"            in new A_SignalInt { unboundFunctionInt(A,C, changeA(8)) }
    it should "cast to Unbound[() => Option[T]]"    in new A_SignalInt { unboundFunctionOptionInt(A,C, changeA(9)) }

    "A value of type () => T" should "cast to () => Option[T]"  in new A_FunctionInt { functionOptionInt(A,C, changeA(10)) }
    it should "cast to Unbound[() => T]"                        in new A_FunctionInt { unboundFunctionInt(A,C, changeA(11)) }
    it should "cast to Unbound[() => Option[T]]"                in new A_FunctionInt { unboundFunctionOptionInt(A,C, changeA(12)) }

    "A value of type Signal[Option[T]]" should "cast to () => Option[T]" in new A_SignalInt {//
        functionOptionInt(someA,C, changeA(13))
    }

    it should "cast to Unbound[Signal[Option[T]]]" in new A_SignalInt { unboundSignalOptionInt(someA,C, changeA(14)) }
    it should "cast to Unbound[() => Option[T]]" in new A_SignalInt { unboundFunctionOptionInt(someA,C, changeA(15)) }

    "A value of type () => Option[T]" should "cast to Unbound[() => Option[T]]" in new A_FunctionInt {
        unboundFunctionOptionInt(someA,C, changeA(16))
    }

    val unboundC = unbound(C)

    "A value of type Unbound[T]" should "cast to Unbound[Option[T]]" in unboundOptionInt(unboundC)
    it should "cast to Unbound[Signal[T]]" in unboundSignalInt(unboundC,C)
    it should "cast to Unbound[Signal[Option[T]]]" in unboundSignalOptionInt(unboundC,C)
    it should "cast to Unbound[() => T]" in unboundFunctionInt(unboundC,C)
    it should "cast to Unbound[() => Option[T]]" in unboundFunctionOptionInt(unboundC,C)

    val unboundOptionC = unbound(Some(2))

    "A value of type Unbound[Option[T]]" should "cast to Unbound[Signal[Option[T]]]" in unboundSignalOptionInt(unboundOptionC,C)
    it should "cast to Unbound[() => Option[T]]" in unboundFunctionOptionInt(unboundOptionC,C)

    "A value of type Unbound[Signal[T]]" should "cast to Unbound[Signal[Option[T]]]" in new A_SignalInt {
        unboundSignalOptionInt(unbound(A),C, changeA(17))
    }

    it should "cast to Unbound[() => T]" in new A_SignalInt { unboundFunctionInt(unboundA,C, changeA(18)) }
    it should "cast to Unbound[() => Option[T]]" in new A_SignalInt { unboundSignalOptionInt(unboundA,C, changeA(19)) }

    "A value of type Unbound[() => T]" should "cast to Unbound[() => Option[T]]" in new A_FunctionInt {
        unboundFunctionOptionInt(unboundA,C, changeA(20))
    }

}
