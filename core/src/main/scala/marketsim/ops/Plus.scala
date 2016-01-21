package marketsim
package ops

import memoization.memo

object Plus {

    /**
     * Typeclass defining plus for elementary types: Int, Long, Double, Duration, Ticks, Currency etc
     */
    trait OnScalar[A,B,R]
    {
        def plus(a : A, b : B) : R
    }

    /**
     * Typeclass defining plus for possibly optional elementary types.
     * Implementations rely that a corresponding OnScalar typeclass is defined for types with removed Option
     */
    trait OnOption[-A,-B,+R]
    {
        val plus: (A, B) => R
    }

    /**
     * Typeclass defining plus for possibly signals and functions over possibly optional elementary types
     * Implementations rely that a corresponding OnOption typeclass
     * is defined for types with removed signal or function type
     */
    trait OnFuncSig[-A,-B,+R]
    {
        val plus: (A, B) => R
    }

    /**
     * Typeclass defining plus for possibly unbound values
     * that possibly signals and functions over possibly optional elementary types
     * Implementations rely that a corresponding OnFuncSig typeclass
     * is defined for types with removed Unbound type
     */
    trait OnUnbound[A,B,R]
    {
        val plus : (A, B) => R
    }

    /**
     * Hack to tell explicitly that typeclasses over signals
     * have higher priority than typeclasses over functions
     * (since the first ones derive from the latter ones)
     */
    trait LowerPriority
    {
        ///////////////////////////////////  Function <-> Constant /////////////////////////////////////

        /**
         *  Standard implementation of difference between two functions
         */
        @memo
        def Func[A, B, R](a: () => A, b: () => B)(implicit m: OnOption[A, B, R]): () => R = {
            CachedFunction(() => m plus(a(), b()))
        }

        @memo
        implicit def plus_functionFunction[A, B, R](implicit m: OnOption[A, B, R])
        : OnFuncSig[() => A, () => B, () => R] =
            new OnFuncSig[() => A, () => B, () => R] {
                val plus = (a: () => A, b: () => B) => Func(a, b)
            }

        @memo
        implicit def plus_functionConstant[A, B: Manifest, R](implicit m: OnOption[A, B, R])
        : OnFuncSig[() => A, B, () => R] =
            new OnFuncSig[() => A, B, () => R] {
                val plus = (a: () => A, b: B) => Func(a, Const(b))
            }

        @memo
        implicit def plus_constantFunction[A: Manifest, B, R](implicit m: OnOption[A, B, R])
        : OnFuncSig[A, () => B, () => R] =
            new OnFuncSig[A, () => B, () => R] {
                val plus = (a: A, b: () => B) => Func(Const(a), b)
            }

    }

    trait Implicits extends LowerPriority {

        ///////////////////////////////////  Elementary <-> Elementary /////////////////////////////////////

        implicit object plus_intInt extends OnScalar[Int, Int, Int] {
            def plus(a: Int, b: Int) = a + b
        }

        implicit object plus_intDouble extends OnScalar[Int, Double, Double] {
            def plus(a: Int, b: Double) = a + b
        }

        implicit object plus_doubleInt extends OnScalar[Double, Int, Double] {
            def plus(a: Double, b: Int) = a + b
        }

        implicit object plus_intLong extends OnScalar[Int, Long, Long] {
            def plus(a: Int, b: Long) = a + b
        }

        implicit object plus_longInt extends OnScalar[Long, Int, Long] {
            def plus(a: Long, b: Int) = a + b
        }

        implicit object plus_longLong extends OnScalar[Long, Long, Long] {
            def plus(a: Long, b: Long) = a + b
        }

        implicit object plus_longDouble extends OnScalar[Long, Double, Double] {
            def plus(a: Long, b: Double) = a + b
        }

        implicit object plus_doubleLong extends OnScalar[Double, Long, Double] {
            def plus(a: Double, b: Long) = a + b
        }

        implicit object plus_doubleDouble extends OnScalar[Double, Double, Double] {
            def plus(a: Double, b: Double) = a + b
        }

        ///////////////////////////////////  possibly Option <-> possibly Option /////////////////////////////////////

        @memo
        implicit def plus_scalarScalar[A, B, R](implicit m: OnScalar[A, B, R]): OnOption[A, B, R] =
            new OnOption[A, B, R] {
                val plus = (a: A, b: B) => m plus(a, b)
            }

        @memo
        implicit def plus_optionOption[A, B, R](implicit m: OnScalar[A, B, R]): OnOption[Option[A], Option[B], Option[R]] =
            new OnOption[Option[A], Option[B], Option[R]] {
                val plus = (a: Option[A], b: Option[B]) => (a, b) match {
                    case (Some(x), Some(y)) => Some(m plus(x, y))
                    case _ => None
                }
            }

        @memo
        implicit def plus_scalarOption[A, B, R](implicit m: OnScalar[A, B, R]): OnOption[A, Option[B], Option[R]] =
            new OnOption[A, Option[B], Option[R]] {
                val plus = (a: A, b: Option[B]) => b match {
                    case Some(y) => Some(m plus(a, y))
                    case _ => None
                }
            }

        @memo
        implicit def plus_optionScalar[A, B, R](implicit m: OnScalar[A, B, R]): OnOption[Option[A], B, Option[R]] =
            new OnOption[Option[A], B, Option[R]] {
                val plus = (a: Option[A], b: B) => a match {
                    case Some(x) => Some(m plus(x, b))
                    case _ => None
                }
            }

        ///////////////////////////////////  const or signal <-> const or signal /////////////////////////////////////

        @memo
        implicit def plus_constConst[A, B, R](implicit m: OnOption[A, B, R]): OnFuncSig[A, B, R] =
            new OnFuncSig[A, B, R] {
                val plus = m.plus
            }

        @memo
        implicit def plus_signalSignal[A, B, R](implicit m: OnOption[A, B, R])
        : OnFuncSig[reactive.Signal[A], reactive.Signal[B], reactive.Signal[R]] =
            new OnFuncSig[reactive.Signal[A], reactive.Signal[B], reactive.Signal[R]] {
                val plus =
                    (a: reactive.Signal[A], b: reactive.Signal[B]) =>
                        reactive.Binary(a, b, "-")(m.plus)
            }

        @memo
        implicit def plus_signalConstant[A, B: Manifest, R](implicit m: OnOption[A, B, R])
        : OnFuncSig[reactive.Signal[A], B, reactive.Signal[R]] =
            new OnFuncSig[reactive.Signal[A], B, reactive.Signal[R]] {
                val plus =
                    (a: reactive.Signal[A], b: B) =>
                        reactive.Binary(a, reactive.Constant(b), "-")(m.plus)
            }

        @memo
        implicit def plus_constantSignal[A: Manifest, B, R](implicit m: OnOption[A, B, R])
        : OnFuncSig[A, reactive.Signal[B], reactive.Signal[R]] =
            new OnFuncSig[A, reactive.Signal[B], reactive.Signal[R]] {
                val plus =
                    (a: A, b: reactive.Signal[B]) =>
                        reactive.Binary(reactive.Constant(a), b, "-")(m.plus)
            }

        ///////////////////////////////////  possibly unbound <-> possibly unbound /////////////////////////////////////


        implicit def plus_boundBound[A, B, R](implicit m: OnFuncSig[A, B, R]): OnUnbound[A, B, R] =
            new OnUnbound[A, B, R] {
                val plus = m.plus
            }

        @memo
        def unboundImpl[A, B, R](a: Unbound[A], b: Unbound[B])(implicit m: OnFuncSig[A, B, R]): Unbound[R] = {
            (ctx: Context) => m plus(a(ctx), b(ctx))
        }

        implicit def plus_unboundUnbound[A, B, R](implicit m: OnFuncSig[A, B, R])
        : OnUnbound[Unbound[A], Unbound[B], Unbound[R]] =
            new OnUnbound[Unbound[A], Unbound[B], Unbound[R]] {
                val plus = (a: Unbound[A], b: Unbound[B]) => unboundImpl(a, b)
            }

        implicit def plus_boundUnbound[A: Manifest, B, R](implicit m: OnFuncSig[A, B, R])
        : OnUnbound[A, Unbound[B], Unbound[R]] =
            new OnUnbound[A, Unbound[B], Unbound[R]] {
                val plus = (a: A, b: Unbound[B]) => unboundImpl(unbound(a), b)
            }

        implicit def plus_unboundBound[A, B: Manifest, R](implicit m: OnFuncSig[A, B, R])
        : OnUnbound[Unbound[A], B, Unbound[R]] =
            new OnUnbound[Unbound[A], B, Unbound[R]] {
                val plus = (a: Unbound[A], b: B) => unboundImpl(a, unbound(b))
            }

        implicit class RichTWithPlus[A](x: A) {
            def +[B, R](y: B)(implicit m: OnUnbound[A, B, R]) = m plus(x, y)
        }

    }
}
