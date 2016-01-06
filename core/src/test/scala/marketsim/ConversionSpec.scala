package marketsim

class ConversionSpec extends EnsureChanges {

    import reactive._

    case class option[R](initial : R) {
        def apply[T](x: T)(implicit c: ConversionUnbound[T, Option[R]]) = {
            val converted = x.as[Option[R]]
            assertResult(Some(initial))(converted)
        }
    }

    case class signal[R](initial : R) {
        def apply[T](x: T)(implicit c: ConversionUnbound[T, Signal[R]]) = {
            val converted = x.as[Signal[R]]
            val converted2 = x.as[Signal[R]]
            assert(converted eq converted2)
            assertResult(initial)(converted())
        }
    }

    case class signalOption[R](initial : R) {
        def apply[T](x: T, changes: (() => Unit, R)*)
                    (implicit c: ConversionUnbound[T, Signal[Option[R]]]) = {
            val converted = x.as[Signal[Option[R]]]
            val converted2 = x.as[Signal[Option[R]]]
            assert(converted eq converted2)
            ensureSignalOption(converted, initial, changes: _*)
        }
    }

    case class function[R](initial : R) {
        def apply[T](x: T, changes: (() => Unit, R)*)
                    (implicit c: ConversionUnbound[T, () => R]) = {
            val converted = x.as[() => R]
            val converted2 = x.as[() => R]
            assert(converted eq converted2)
            ensureFunction(converted, initial, changes: _*)
        }
    }

    case class functionOption[R](initial: R) {
        def apply[T](x: T, changes: (() => Unit, R)*)
                    (implicit c: ConversionUnbound[T, () => Option[R]]) = {
            val converted = x.as[() => Option[R]]
            val converted2 = x.as[() => Option[R]]
            assert(converted eq converted2)
            ensureFunctionOption(converted, initial, changes: _*)
        }
    }

    case class unboundT[R](initial : R) {
        def apply[T](x: T)(implicit c: ConversionUnbound[T, Unbound[Int]]) = {
            val converted = x.as[Unbound[Int]]
            val converted2 = x.as[Unbound[Int]]
            assert(converted eq converted2)
            assertResult(initial)(converted(ctx))
        }
    }

    case class unboundOption[R](initial : R) {
        def apply[T](x : T)(implicit c : ConversionUnbound[T, Unbound[Option[Int]]]) = {
            val converted = x.as[Unbound[Option[Int]]]
            val converted2 = x.as[Unbound[Option[Int]]]
            assert(converted eq converted2)
            assertResult(Some(initial))(converted(ctx))
        }
    }

    case class unboundSignal[R](initial : R) {
        def apply[T](x: T, changes: (() => Unit, R)*)
                    (implicit c: ConversionUnbound[T, Unbound[Signal[R]]]) = {
            val converted = x.as[Unbound[Signal[R]]]
            val converted2 = x.as[Unbound[Signal[R]]]
            assert(converted eq converted2)
            val bound = converted(ctx)

            ensureSignal(bound, initial, changes: _*)
        }
    }

    case class unboundSignalOption[R](initial: R) {
        def apply[T](x: T, changes: (() => Unit, R)*)
                    (implicit c: ConversionUnbound[T, Unbound[Signal[Option[R]]]]) = {
            val converted = x.as[Unbound[Signal[Option[R]]]]
            val converted2 = x.as[Unbound[Signal[Option[R]]]]
            assert(converted eq converted2)
            val bound = converted(ctx)

            ensureSignalOption(bound, initial, changes: _*)
        }
    }

    case class unboundFunction[R](initial : R) {
        def apply[T](x: T, changes: (() => Unit, R)*)
                    (implicit c: ConversionUnbound[T, Unbound[() => R]]) = {
            val converted = x.as[Unbound[() => R]]
            val converted2 = x.as[Unbound[() => R]]
            assert(converted eq converted2)
            val bound = converted(ctx)

            ensureFunction(bound, initial, changes: _*)
        }
    }

    case class unboundFunctionOption[R](initial: R) {
        def apply[T](x: T, changes: (() => Unit, R)*)
                    (implicit c: ConversionUnbound[T, Unbound[() => Option[R]]]) = {
            val converted = x.as[Unbound[() => Option[R]]]
            val converted2 = x.as[Unbound[() => Option[R]]]
            assert(converted eq converted2)
            val bound = converted(ctx)

            ensureFunctionOption(bound, initial, changes: _*)
        }
    }

    import conversions._

    "A value of type Int" should "cast to Option[Int]"  in option[Int](C)(C)
    it should "cast to Option[Double]"                  in option[Double](C)(C)
    it should "cast to Signal[Int]"                     in signal[Int](C)(C)
    it should "cast to Signal[Double]"                  in signal[Double](C)(C)
    it should "cast to Signal[Option[Int]]"           in signalOption[Int](C)(C)
    it should "cast to Signal[Option[Double]]"           in signalOption[Double](C)(C)
    it should "cast to Function[Int]"                 in function[Int](C)(C)
    it should "cast to Function[Double]"                 in function[Double](C)(C)
    it should "cast to Function[Option[Int]]"         in functionOption[Int](C)(C)
    it should "cast to Function[Option[Double]]"         in functionOption[Double](C)(C)
    it should "cast to Unbound[Int]"                  in unboundT[Int](C)(C)
    it should "cast to Unbound[Double]"             in unboundT[Double](C)(C)
    it should "cast to Unbound[Option[Int]]"          in unboundOption[Int](C)(C)
    it should "cast to Unbound[Option[Double]]"          in unboundOption[Double](C)(C)
    it should "cast to Unbound[Signal[Int]]"          in unboundSignal[Int](C)(C)
    it should "cast to Unbound[Signal[Double]]"          in unboundSignal[Double](C)(C)
    it should "cast to Unbound[Signal[Option[Int]]]"  in unboundSignalOption[Int](C)(C)
    it should "cast to Unbound[Signal[Option[Double]]]"  in unboundSignalOption[Double](C)(C)
    it should "cast to Unbound[() => Int]"            in unboundFunction[Int](C)(C)
    it should "cast to Unbound[() => Double]"            in unboundFunction[Double](C)(C)
    it should "cast to Unbound[() => Option[Int]]"    in unboundFunctionOption[Int](C)(C)
    it should "cast to Unbound[() => Option[Double]]"    in unboundFunctionOption[Double](C)(C)

    val someC = Some(C) : Option[Int]

    "A value of type Option[Int]" should "cast to Signal[Option[Int]]"  in signalOption[Int](C)(someC)
    it should "cast to Signal[Option[Double]]"  in signalOption[Double](C)(someC)
    it should "cast to () => Option[Int]"                             in functionOption[Int](C)(someC)
    it should "cast to () => Option[Double]"                             in functionOption[Double](C)(someC)
    it should "cast to Unbound[Option[Int]]"                          in unboundOption[Int](C)(someC)
    it should "cast to Unbound[Option[Double]]"                          in unboundOption[Double](C)(someC)
    it should "cast to Unbound[Signal[Option[Int]]]"                  in unboundSignalOption[Int](C)(someC)
    it should "cast to Unbound[Signal[Option[Double]]]"                  in unboundSignalOption[Double](C)(someC)
    it should "cast to Unbound[() => Option[Int]]"                    in unboundFunctionOption[Int](C)(someC)
    it should "cast to Unbound[() => Option[Double]]"                    in unboundFunctionOption[Double](C)(someC)

    "A value of type Signal[Int]" should "cast to Signal[Option[Int]]" in new A_SignalInt { signalOption[Int](C)(A, changeA(3)) }
    it should "cast to Signal[Option[Double]]" in new A_SignalInt { signalOption[Double](C)(A, changeA(3, 3.0)) }
    it should "cast to Function[Int]"                 in new A_SignalInt { function[Int](C)(A,changeA(4)) }
    it should "cast to Function[Double]"                 in new A_SignalInt { function[Double](C)(A,changeA(4, 4.0)) }
    it should "cast to Function[Option[Int]]"         in new A_SignalInt { functionOption[Int](C)(A, changeA(5)) }
    it should "cast to Function[Option[Double]]"         in new A_SignalInt { functionOption[Double](C)(A, changeA(5, 5.0)) }
    it should "cast to Unbound[Signal[Int]]"          in new A_SignalInt { unboundSignal[Int](C)(A,changeA(6)) }
    it should "cast to Unbound[Signal[Double]]"          in new A_SignalInt { unboundSignal[Double](C)(A,changeA(6,6.0)) }
    it should "cast to Unbound[Signal[Option[Int]]]"  in new A_SignalInt { unboundSignalOption[Int](C)(A,changeA(7)) }
    it should "cast to Unbound[Signal[Option[Double]]]"  in new A_SignalInt { unboundSignalOption[Double](C)(A,changeA(7,7.0)) }
    it should "cast to Unbound[() => Int]"            in new A_SignalInt { unboundFunction[Int](C)(A,changeA(8)) }
    it should "cast to Unbound[() => Double]"            in new A_SignalInt { unboundFunction[Double](C)(A,changeA(8,8.0)) }
    it should "cast to Unbound[() => Option[Int]]"    in new A_SignalInt { unboundFunctionOption[Int](C)(A,changeA(9)) }
    it should "cast to Unbound[() => Option[Double]]"    in new A_SignalInt { unboundFunctionOption[Double](C)(A,changeA(9,9.0)) }

    "A value of type () => Int" should "cast to () => Option[Int]"  in new A_FunctionInt { functionOption[Int](C)(A,changeA(10)) }
    it should "cast to () => Option[Double]"  in new A_FunctionInt { functionOption[Double](C)(A,changeAx(10, 10.0)) }
    it should "cast to Unbound[() => Int]"                        in new A_FunctionInt { unboundFunction[Int](C)(A,changeA(11)) }
    it should "cast to Unbound[() => Double]"                        in new A_FunctionInt { unboundFunction[Double](C)(A,changeAx(11, 11.0)) }
    it should "cast to Unbound[() => Option[Int]]"                in new A_FunctionInt { unboundFunctionOption[Int](C)(A, changeA(12)) }
    it should "cast to Unbound[() => Option[Double]]"                in new A_FunctionInt { unboundFunctionOption[Double](C)(A, changeAx(12, 12.0)) }

    "A value of type Signal[Option[Int]]" should "cast to () => Option[Int]" in new A_SignalInt {
        functionOption[Int](C)(someA,changeA(13))
    }
/*    it should "cast to () => Option[Double]" in new A_SignalInt {
        functionOption[Double](C)(someA,changeA(13, 13.0))
    }*/

    it should "cast to Unbound[Signal[Option[Int]]]" in new A_SignalInt { unboundSignalOption[Int](C)(someA,changeA(14)) }
    it should "cast to Unbound[() => Option[Int]]" in new A_SignalInt { unboundFunctionOption[Int](C)(someA,changeA(15)) }

    "A value of type () => Option[Int]" should "cast to Unbound[() => Option[Int]]" in new A_FunctionInt {
        unboundFunctionOption[Int](C)(someA,changeA(16))
    }

    val unboundC = unbound(C)

    "A value of type Unbound[Int]" should "cast to Unbound[Option[Int]]" in unboundOption[Int](C)(unboundC)
    it should "cast to Unbound[Signal[Int]]" in unboundSignal[Int](C)(unboundC)
    it should "cast to Unbound[Signal[Option[Int]]]" in unboundSignalOption[Int](C)(unboundC)
    it should "cast to Unbound[() => Int]" in unboundFunction[Int](C)(unboundC)
    it should "cast to Unbound[() => Option[Int]]" in unboundFunctionOption[Int](C)(unboundC)

    val unboundOptionC = unbound(Some(2))

    "A value of type Unbound[Option[Int]]" should "cast to Unbound[Signal[Option[Int]]]" in unboundSignalOption[Int](C)(unboundOptionC)
    it should "cast to Unbound[() => Option[Int]]" in unboundFunctionOption[Int](C)(unboundOptionC)

    "A value of type Unbound[Signal[Int]]" should "cast to Unbound[Signal[Option[Int]]]" in new A_SignalInt {
        unboundSignalOption[Int](C)(unbound(A),changeA(17))
    }

    it should "cast to Unbound[() => Int]" in new A_SignalInt { unboundFunction[Int](C)(unboundA, changeA(18)) }
    it should "cast to Unbound[() => Option[Int]]" in new A_SignalInt { unboundSignalOption[Int](C)(unboundA,changeA(19)) }

    "A value of type Unbound[() => Int]" should "cast to Unbound[() => Option[Int]]" in new A_FunctionInt {
        unboundFunctionOption[Int](C)(unboundA,changeA(20))
    }

    def optCast[R] = new { def apply[T](x : T)(implicit c : ConversionOpt[T,R]) = c convert x }

    "Int" should "optCast to Option[Double]" in assertResult(optCast[Option[Double]](1))(Some(1.0))
    "Int" should "optCast to Double" in assertResult(optCast[Double](1))(1.0)
    "Option[Int]" should "optCast to Option[Double]" in assertResult(optCast[Option[Double]](Some(1)))(Some(1.0))

    def fsCast[R] = new { def apply[T](x : T)(implicit c : ConversionFuncSig[T,R]) = c convert x }

    "Int" should "fsCast to Option[Double]" in assertResult(fsCast[Option[Double]](1))(Some(1.0))
    "Int" should "fsCast to Double" in assertResult(fsCast[Double](1))(1.0)
    "Option[Int]" should "fsCast to Option[Double]" in assertResult(fsCast[Option[Double]](Some(1)))(Some(1.0))

    "() => Int" should "fsCast to () => Option[Double]" in
        assertResult(fsCast[() => Option[Double]](() => 1).apply())(Some(1.0))

    "() => Int" should "fsCast to () => Double" in
        assertResult(fsCast[() => Double](() => 1).apply())(1.0)

    "() => Option[Int]" should "fsCast to () => Option[Double]" in
        assertResult(fsCast[() => Option[Double]](() => Some(1)).apply())(Some(1.0))

    "Int" should "fsCast to () => Option[Double]" in
        assertResult(fsCast[() => Option[Double]](1).apply())(Some(1.0))

    "Int" should "fsCast to () => Double" in
        assertResult(fsCast[() => Double](1).apply())(1.0)

    "Option[Int]" should "fsCast to () => Option[Double]" in
        assertResult(fsCast[() => Option[Double]](Some(1)).apply())(Some(1.0))

    "Signal[Int]" should "fsCast to Signal[Option[Double]]" in
        assertResult(fsCast[Signal[Option[Double]]](Constant(1)).apply())(Some(1.0))

    "Signal[Int]" should "fsCast to Signal[Double]" in
        assertResult(fsCast[Signal[Double]](Constant(1)).apply())(1.0)

    "Signal[Option[Int]]" should "fsCast to Signal[Option[Double]]" in
        assertResult(fsCast[Signal[Option[Double]]](Constant(Some(1))).apply())(Some(1.0))

    "Int" should "fsCast to Signal[Option[Double]]" in
        assertResult(fsCast[() => Option[Double]](1).apply())(Some(1.0))

    "Int" should "fsCast to Signal[Double]" in
        assertResult(fsCast[Signal[Double]](1).apply())(1.0)

    "Option[Int]" should "fsCast to Signal[Option[Double]]" in
        assertResult(fsCast[Signal[Option[Double]]](Some(1)).apply())(Some(1.0))

    "Signal[Int]" should "fsCast to () => Option[Double]" in
        assertResult(fsCast[() => Option[Double]](Constant(1)).apply())(Some(1.0))

    "Signal[Int]" should "fsCast to () => Double" in
        assertResult(fsCast[() => Double](Constant(1)).apply())(1.0)

    "Signal[Option[Int]]" should "fsCast to () => Option[Double]" in
        assertResult(fsCast[() => Option[Double]](Constant(Some(1))).apply())(Some(1.0))

}
