package marketsim
package ops

import memoization.memo

object And {
    
    /**
     * Typeclass defining and for possibly optional elementary types.
     */
    trait OnOption[-A,-B,+R]
    {
        val and: (A, B) => R
    }

    /**
     * Typeclass defining and for possibly signals and functions over possibly optional elementary types
     * Implementations rely that a corresponding OnOption typeclass
     * is defined for types with removed signal or function type
     */
    trait OnFuncSig[-A,-B,+R]
    {
        val and: (A, B) => R
    }

    /**
     * Typeclass defining and for possibly unbound values
     * that possibly signals and functions over possibly optional elementary types
     * Implementations rely that a corresponding OnFuncSig typeclass
     * is defined for types with removed Unbound type
     */
    trait OnUnbound[A,B,R]
    {
        val and : (A, B) => R
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
            CachedFunction(() => m and(a(), b()))
        }

        @memo
        implicit def and_functionFunction[A, B, R](implicit m: OnOption[A, B, R])
        : OnFuncSig[() => A, () => B, () => R] =
            new OnFuncSig[() => A, () => B, () => R] {
                val and = (a: () => A, b: () => B) => Func(a, b)
            }

        @memo
        implicit def and_functionConstant[A, B: Manifest, R](implicit m: OnOption[A, B, R])
        : OnFuncSig[() => A, B, () => R] =
            new OnFuncSig[() => A, B, () => R] {
                val and = (a: () => A, b: B) => Func(a, Const(b))
            }

        @memo
        implicit def and_constantFunction[A: Manifest, B, R](implicit m: OnOption[A, B, R])
        : OnFuncSig[A, () => B, () => R] =
            new OnFuncSig[A, () => B, () => R] {
                val and = (a: A, b: () => B) => Func(Const(a), b)
            }

    }

    trait Implicits extends LowerPriority {


        ///////////////////////////////////  possibly Option <-> possibly Option /////////////////////////////////////

        @memo
        implicit def and_scalarScalar: OnOption[Boolean, Boolean, Boolean] =
            new OnOption[Boolean, Boolean, Boolean] {
                val and = (a: Boolean, b: Boolean) => a && b
            }

        @memo
        implicit def and_optionOption: OnOption[Option[Boolean], Option[Boolean], Option[Boolean]] =
            new OnOption[Option[Boolean], Option[Boolean], Option[Boolean]] {
                val and = (a: Option[Boolean], b: Option[Boolean]) => (a, b) match {
                    case (Some(x), Some(y)) => Some(x && y)
                    case _ => None
                }
            }

        @memo
        implicit def and_scalarOption: OnOption[Boolean, Option[Boolean], Option[Boolean]] =
            new OnOption[Boolean, Option[Boolean], Option[Boolean]] {
                val and = (a: Boolean, b: Option[Boolean]) => b match {
                    case Some(y) => Some(a && y)
                    case _ => None
                }
            }

        @memo
        implicit def and_optionScalar: OnOption[Option[Boolean], Boolean, Option[Boolean]] =
            new OnOption[Option[Boolean], Boolean, Option[Boolean]] {
                val and = (a: Option[Boolean], b: Boolean) => a match {
                    case Some(x) => Some(x && b)
                    case _ => None
                }
            }

        ///////////////////////////////////  const or signal <-> const or signal /////////////////////////////////////

        @memo
        implicit def and_constConst[A, B, R](implicit m: OnOption[A, B, R]): OnFuncSig[A, B, R] =
            new OnFuncSig[A, B, R] {
                val and = m.and
            }

        @memo
        implicit def and_signalSignal[A, B, R](implicit m: OnOption[A, B, R])
        : OnFuncSig[reactive.Signal[A], reactive.Signal[B], reactive.Signal[R]] =
            new OnFuncSig[reactive.Signal[A], reactive.Signal[B], reactive.Signal[R]] {
                val and =
                    (a: reactive.Signal[A], b: reactive.Signal[B]) =>
                        reactive.Binary(a,b, "&&")(m.and)
            }

        @memo
        implicit def and_signalConstant[A, B: Manifest, R](implicit m: OnOption[A, B, R])
        : OnFuncSig[reactive.Signal[A], B, reactive.Signal[R]] =
            new OnFuncSig[reactive.Signal[A], B, reactive.Signal[R]] {
                val and =
                    (a: reactive.Signal[A], b: B) =>
                        reactive.Binary(a, reactive.Constant(b), "&&")(m.and)
            }

        @memo
        implicit def and_constantSignal[A: Manifest, B, R](implicit m: OnOption[A, B, R])
        : OnFuncSig[A, reactive.Signal[B], reactive.Signal[R]] =
            new OnFuncSig[A, reactive.Signal[B], reactive.Signal[R]] {
                val and =
                    (a: A, b: reactive.Signal[B]) =>
                        reactive.Binary(reactive.Constant(a), b, "&&")(m.and)
            }

        ///////////////////////////////////  possibly unbound <-> possibly unbound /////////////////////////////////////


        implicit def and_boundBound[A, B, R](implicit m: OnFuncSig[A, B, R]): OnUnbound[A, B, R] =
            new OnUnbound[A, B, R] {
                val and = m.and
            }

        @memo
        def unboundImpl[A, B, R](a: Unbound[A], b: Unbound[B])(implicit m: OnFuncSig[A, B, R]): Unbound[R] = {
            (ctx: Context) => m and(a(ctx), b(ctx))
        }

        implicit def and_unboundUnbound[A, B, R](implicit m: OnFuncSig[A, B, R])
        : OnUnbound[Unbound[A], Unbound[B], Unbound[R]] =
            new OnUnbound[Unbound[A], Unbound[B], Unbound[R]] {
                val and = (a: Unbound[A], b: Unbound[B]) => unboundImpl(a, b)
            }

        implicit def and_boundUnbound[A: Manifest, B, R](implicit m: OnFuncSig[A, B, R])
        : OnUnbound[A, Unbound[B], Unbound[R]] =
            new OnUnbound[A, Unbound[B], Unbound[R]] {
                val and = (a: A, b: Unbound[B]) => unboundImpl(unbound(a), b)
            }

        implicit def and_unboundBound[A, B: Manifest, R](implicit m: OnFuncSig[A, B, R])
        : OnUnbound[Unbound[A], B, Unbound[R]] =
            new OnUnbound[Unbound[A], B, Unbound[R]] {
                val and = (a: Unbound[A], b: B) => unboundImpl(a, unbound(b))
            }

        implicit class RichTWithAnd[A](x: A) {
            def &&[B, R](y: B)(implicit m: OnUnbound[A, B, R]) = m and(x, y)
            def And[B, R](y: B)(implicit m: OnUnbound[A, B, R]) = m and(x, y)
        }

    }
}
