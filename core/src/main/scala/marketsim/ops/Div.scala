package marketsim
package ops

import memoization.memo

object Div {

    /**
     * Typeclass defining div for elementary types: Int, Long, Double, Duration, Ticks, Currency etc
     */
    trait OnScalar[A,B,R]
    {
        def div(a : A, b : B) : R
    }

    /**
     * Typeclass defining div for possibly optional elementary types.
     * Implementations rely that a corresponding OnScalar typeclass is defined for types with removed Option
     */
    trait OnOption[-A,-B,+R]
    {
        val div: (A, B) => R
    }

    /**
     * Typeclass defining div for possibly signals and functions over possibly optional elementary types
     * Implementations rely that a corresponding OnOption typeclass
     * is defined for types with removed signal or function type
     */
    trait OnFuncSig[-A,-B,+R]
    {
        val div: (A, B) => R
    }

    /**
     * Typeclass defining div for possibly unbound values
     * that possibly signals and functions over possibly optional elementary types
     * Implementations rely that a corresponding OnFuncSig typeclass
     * is defined for types with removed Unbound type
     */
    trait OnUnbound[A,B,R]
    {
        val div : (A, B) => R
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
            CachedFunction(() => m div(a(), b()))
        }

        @memo
        implicit def div_functionFunction[A, B, R](implicit m: OnOption[A, B, R])
        : OnFuncSig[() => A, () => B, () => R] =
            new OnFuncSig[() => A, () => B, () => R] {
                val div = (a: () => A, b: () => B) => Func(a, b)
            }

        @memo
        implicit def div_functionConstant[A, B: Manifest, R](implicit m: OnOption[A, B, R])
        : OnFuncSig[() => A, B, () => R] =
            new OnFuncSig[() => A, B, () => R] {
                val div = (a: () => A, b: B) => Func(a, Const(b))
            }

        @memo
        implicit def div_constantFunction[A: Manifest, B, R](implicit m: OnOption[A, B, R])
        : OnFuncSig[A, () => B, () => R] =
            new OnFuncSig[A, () => B, () => R] {
                val div = (a: A, b: () => B) => Func(Const(a), b)
            }

    }

    trait Implicits extends LowerPriority {

        ///////////////////////////////////  Elementary <-> Elementary /////////////////////////////////////

        implicit object div_intInt extends OnScalar[Int, Int, Int] {
            def div(a: Int, b: Int) = a / b
        }

        implicit object div_intDouble extends OnScalar[Int, Double, Double] {
            def div(a: Int, b: Double) = a / b
        }

        implicit object div_doubleInt extends OnScalar[Double, Int, Double] {
            def div(a: Double, b: Int) = a / b
        }

        implicit object div_intLong extends OnScalar[Int, Long, Long] {
            def div(a: Int, b: Long) = a / b
        }

        implicit object div_longInt extends OnScalar[Long, Int, Long] {
            def div(a: Long, b: Int) = a / b
        }

        implicit object div_longLong extends OnScalar[Long, Long, Long] {
            def div(a: Long, b: Long) = a / b
        }

        implicit object div_longDouble extends OnScalar[Long, Double, Double] {
            def div(a: Long, b: Double) = a / b
        }

        implicit object div_doubleLong extends OnScalar[Double, Long, Double] {
            def div(a: Double, b: Long) = a / b
        }

        implicit object div_doubleDouble extends OnScalar[Double, Double, Double] {
            def div(a: Double, b: Double) = a / b
        }

        ///////////////////////////////////  possibly Option <-> possibly Option /////////////////////////////////////

        @memo
        implicit def div_scalarScalar[A, B, R](implicit m: OnScalar[A, B, R]): OnOption[A, B, R] =
            new OnOption[A, B, R] {
                val div = (a: A, b: B) => m div(a, b)
            }

        @memo
        implicit def div_optionOption[A, B, R](implicit m: OnScalar[A, B, R]): OnOption[Option[A], Option[B], Option[R]] =
            new OnOption[Option[A], Option[B], Option[R]] {
                val div = (a: Option[A], b: Option[B]) => (a, b) match {
                    case (Some(x), Some(y)) if y != 0 => Some(m div(x, y))
                    case _ => None
                }
            }

        @memo
        implicit def div_scalarOption[A, B, R](implicit m: OnScalar[A, B, R]): OnOption[A, Option[B], Option[R]] =
            new OnOption[A, Option[B], Option[R]] {
                val div = (a: A, b: Option[B]) => b match {
                    case Some(y) if y != 0 => Some(m div(a, y))
                    case _ => None
                }
            }

        @memo
        implicit def div_optionScalar[A, B, R](implicit m: OnScalar[A, B, R]): OnOption[Option[A], B, Option[R]] =
            new OnOption[Option[A], B, Option[R]] {
                val div = (a: Option[A], b: B) => a match {
                    case Some(x) if b != 0 => Some(m div(x, b))
                    case _ => None
                }
            }

        ///////////////////////////////////  const or signal <-> const or signal /////////////////////////////////////

        @memo
        implicit def div_constConst[A, B, R](implicit m: OnOption[A, B, R]): OnFuncSig[A, B, R] =
            new OnFuncSig[A, B, R] {
                val div = m.div
            }

        @memo
        implicit def div_signalSignal[A, B, R](implicit m: OnOption[A, B, R])
        : OnFuncSig[reactive.Signal[A], reactive.Signal[B], reactive.Signal[R]] =
            new OnFuncSig[reactive.Signal[A], reactive.Signal[B], reactive.Signal[R]] {
                val div =
                    (a: reactive.Signal[A], b: reactive.Signal[B]) =>
                        reactive.Binary(a, b, "-")(m.div)
            }

        @memo
        implicit def div_signalConstant[A, B: Manifest, R](implicit m: OnOption[A, B, R])
        : OnFuncSig[reactive.Signal[A], B, reactive.Signal[R]] =
            new OnFuncSig[reactive.Signal[A], B, reactive.Signal[R]] {
                val div =
                    (a: reactive.Signal[A], b: B) =>
                        reactive.Binary(a, reactive.Constant(b), "-")(m.div)
            }

        @memo
        implicit def div_constantSignal[A: Manifest, B, R](implicit m: OnOption[A, B, R])
        : OnFuncSig[A, reactive.Signal[B], reactive.Signal[R]] =
            new OnFuncSig[A, reactive.Signal[B], reactive.Signal[R]] {
                val div =
                    (a: A, b: reactive.Signal[B]) =>
                        reactive.Binary(reactive.Constant(a), b, "-")(m.div)
            }

        ///////////////////////////////////  possibly unbound <-> possibly unbound /////////////////////////////////////


        implicit def div_boundBound[A, B, R](implicit m: OnFuncSig[A, B, R]): OnUnbound[A, B, R] =
            new OnUnbound[A, B, R] {
                val div = m.div
            }

        @memo
        def unboundImpl[A, B, R](a: Unbound[A], b: Unbound[B])(implicit m: OnFuncSig[A, B, R]): Unbound[R] = {
            (ctx: Context) => m div(a(ctx), b(ctx))
        }

        implicit def div_unboundUnbound[A, B, R](implicit m: OnFuncSig[A, B, R])
        : OnUnbound[Unbound[A], Unbound[B], Unbound[R]] =
            new OnUnbound[Unbound[A], Unbound[B], Unbound[R]] {
                val div = (a: Unbound[A], b: Unbound[B]) => unboundImpl(a, b)
            }

        implicit def div_boundUnbound[A: Manifest, B, R](implicit m: OnFuncSig[A, B, R])
        : OnUnbound[A, Unbound[B], Unbound[R]] =
            new OnUnbound[A, Unbound[B], Unbound[R]] {
                val div = (a: A, b: Unbound[B]) => unboundImpl(unbound(a), b)
            }

        implicit def div_unboundBound[A, B: Manifest, R](implicit m: OnFuncSig[A, B, R])
        : OnUnbound[Unbound[A], B, Unbound[R]] =
            new OnUnbound[Unbound[A], B, Unbound[R]] {
                val div = (a: Unbound[A], b: B) => unboundImpl(a, unbound(b))
            }

        implicit class RichTWithDiv[A](x: A) {
            def /[B, R](y: B)(implicit m: OnUnbound[A, B, R]) = m div(x, y)
        }

    }
}
