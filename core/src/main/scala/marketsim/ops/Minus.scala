package marketsim
package ops

import memoization.memo

object Minus {

    trait OnScalar[A,B,R]
    {
        def minus(a : A, b : B) : R
    }

    implicit object intInt extends OnScalar[Int, Int, Int]
    {
        def minus(a : Int, b : Int) = a - b
    }

    implicit object doubleDouble extends OnScalar[Double, Double, Double]
    {
        def minus(a : Double, b : Double) = a - b
    }

    implicit object intDouble extends OnScalar[Int, Double, Double]
    {
        def minus(a : Int, b : Double) = a - b
    }

    implicit object doubleInt extends OnScalar[Double, Int, Double]
    {
        def minus(a : Double, b : Int) = a - b
    }

    trait OnOption[-A,-B,+R]
    {
        val minus: (A, B) => R
    }

    @memo
    implicit def scalarScalar[A,B,R](implicit m : OnScalar[A,B,R]) : OnOption[A,B,R] =
        new OnOption[A,B,R]
        {
            val minus = (a : A, b : B) => m minus (a,b)
        }

    @memo
    implicit def optionOption[A,B,R](implicit m : OnScalar[A,B,R]) : OnOption[Option[A],Option[B],Option[R]] =
        new OnOption[Option[A],Option[B],Option[R]]
        {
            val minus = (a : Option[A], b : Option[B]) => (a,b) match {
                case (Some(x), Some(y)) => Some(m minus (x,y))
                case _ => None
            }
        }

    @memo
    implicit def scalarOption[A,B,R](implicit m : OnScalar[A,B,R]) : OnOption[A,Option[B],Option[R]] =
        new OnOption[A,Option[B],Option[R]]
        {
            val minus = (a : A, b : Option[B]) => b match {
                case Some(y) => Some(m minus (a,y))
                case _ => None
            }
        }

    @memo
    implicit def optionScalar[A,B,R](implicit m : OnScalar[A,B,R]) : OnOption[Option[A],B,Option[R]] =
        new OnOption[Option[A],B,Option[R]]
        {
            val minus = (a : Option[A], b : B) => a match {
                case Some(x) => Some(m minus (x,b))
                case _ => None
            }
        }

    trait OnFuncSig[A,B,R]
    {
        val minus: (A, B) => R
    }

    @memo
    implicit def constConst[A,B,R](implicit m : OnOption[A,B,R]) : OnFuncSig[A,B,R] =
        new OnFuncSig[A,B,R]
        {
            val minus = m.minus
        }

    @memo
    implicit def signalSignal[A,B,R](implicit m : OnOption[A,B,R])
        : OnFuncSig[reactive.Signal[A],reactive.Signal[B],reactive.Signal[R]] =
        new OnFuncSig[reactive.Signal[A],reactive.Signal[B],reactive.Signal[R]]
        {
            val minus =
                (a : reactive.Signal[A], b : reactive.Signal[B]) =>
                    reactive.Binary(a,b,"-")(m.minus)
        }

    @memo
    implicit def signalConstant[A,B : Manifest,R](implicit m : OnOption[A,B,R])
    : OnFuncSig[reactive.Signal[A],B,reactive.Signal[R]] =
        new OnFuncSig[reactive.Signal[A],B,reactive.Signal[R]]
        {
            val minus =
                (a : reactive.Signal[A], b : B) =>
                    reactive.Binary(a,reactive.Constant(b),"-")(m.minus)
        }

    @memo
    implicit def constantSignal[A : Manifest,B,R](implicit m : OnOption[A,B,R])
    : OnFuncSig[A,reactive.Signal[B],reactive.Signal[R]] =
        new OnFuncSig[A,reactive.Signal[B],reactive.Signal[R]]
        {
            val minus =
                (a : A, b : reactive.Signal[B]) =>
                    reactive.Binary(reactive.Constant(a),b,"-")(m.minus)
        }

    @memo
    def Func[A,B,R](a : () => A, b : () => B)(implicit m : OnOption[A,B,R]) : () => R =
    {
        () => m minus(a(), b())
    }

    @memo
    implicit def functionFunction[A,B,R](implicit m : OnOption[A,B,R])
    : OnFuncSig[() => A,() => B,() => R] =
        new OnFuncSig[() => A,() => B,() => R]
        {
            val minus = (a : () => A, b : () => B) => Func(a,b)
        }

    @memo
    implicit def functionConstant[A,B : Manifest,R](implicit m : OnOption[A,B,R])
    : OnFuncSig[() => A,B,() => R] =
        new OnFuncSig[() => A,B,() => R]
        {
            val minus = (a : () => A, b : B) => Func(a, Const(b))
        }

    @memo
    implicit def constantFunction[A : Manifest,B,R](implicit m : OnOption[A,B,R])
    : OnFuncSig[A,() => B,() => R] =
        new OnFuncSig[A,() => B,() => R]
        {
            val minus = (a : A, b : () => B) => Func(Const(a), b)
        }

    @memo
    implicit def functionSignal[A,B,R](implicit m : OnOption[A,B,R])
    : OnFuncSig[() => A,reactive.Signal[B],() => R] =
        new OnFuncSig[() => A,reactive.Signal[B],() => R]
        {
            val minus = (a : () => A, b : reactive.Signal[B]) => Func(a,b)
        }

    @memo
    implicit def signalFunction[A,B,R](implicit m : OnOption[A,B,R])
    : OnFuncSig[reactive.Signal[A],() => B,() => R] =
        new OnFuncSig[reactive.Signal[A],() => B,() => R]
        {
            val minus = (a : reactive.Signal[A], b : () => B) => Func(a,b)
        }

    trait OnUnbound[A,B,R]
    {
        val minus : (A, B) => R
    }

    implicit def boundBound[A,B,R](implicit m : OnFuncSig[A,B,R]) : OnUnbound[A,B,R] =
        new OnUnbound[A,B,R]
        {
            val minus = m.minus
        }

    @memo
    def unboundImpl[A,B,R](a : Unbound[A], b : Unbound[B])(implicit m : OnFuncSig[A,B,R]) : Unbound[R] =
    {
        (ctx : Context) => m minus(a(ctx), b(ctx))
    }

    implicit def unboundUnbound[A,B,R](implicit m : OnFuncSig[A,B,R]) : OnUnbound[Unbound[A],Unbound[B],Unbound[R]] =
        new OnUnbound[Unbound[A],Unbound[B],Unbound[R]]
        {
            val minus = (a : Unbound[A], b : Unbound[B]) => unboundImpl(a,b)
        }

    implicit def boundUnbound[A : Manifest,B,R](implicit m : OnFuncSig[A,B,R]) : OnUnbound[A,Unbound[B],Unbound[R]] =
        new OnUnbound[A,Unbound[B],Unbound[R]]
        {
            val minus = (a : A, b : Unbound[B]) => unboundImpl(unbound(a),b)
        }

    implicit def unboundBound[A,B : Manifest,R](implicit m : OnFuncSig[A,B,R]) : OnUnbound[Unbound[A],B,Unbound[R]] =
        new OnUnbound[Unbound[A],B,Unbound[R]]
        {
            val minus = (a : Unbound[A], b : B) => unboundImpl(a,unbound(b))
        }

}
