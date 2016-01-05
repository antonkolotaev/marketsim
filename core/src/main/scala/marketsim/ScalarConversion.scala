package marketsim

import memoization.memo

import scala.language.implicitConversions

trait ScalarConversion[-From, To] {
    def convert(from: From): To
}

object ScalarConversion {

    implicit def id[T] : ScalarConversion[T,T] = new ScalarConversion[T,T] {
        def convert(x : T) = x
    }

    implicit object int2long extends ScalarConversion[Int, Long] {
        def convert(x : Int) = x
    }

    implicit object int2double extends ScalarConversion[Int, Double] {
        def convert(x : Int) = x
    }

    implicit object long2double extends ScalarConversion[Long, Double] {
        def convert(x : Long) = x
    }

    val a = implicitly[ScalarConversion[Int, Int]].convert(2)

    implicit def idOpt[T,R](implicit s : ScalarConversion[T,R]): ConversionOpt[T, R] =
        new ConversionOpt[T, R] {
            def convert(x: T) : R = {
                s convert x
            }
        }

    implicit def toOpt[T,R](implicit s : ScalarConversion[T,R]): ConversionOpt[T, Option[R]] =
        new ConversionOpt[T, Option[R]] {
            def convert(x: T) : Option[R] = {
                Some(s convert x)
            }
        }

    implicit def betweenOpt[T,R](implicit s : ScalarConversion[T,R]): ConversionOpt[Option[T], Option[R]] =
        new ConversionOpt[Option[T], Option[R]] {
            def convert(x: Option[T]) : Option[R] = {
                x map { s.convert }
            }
        }

    val b = implicitly[ConversionOpt[Int, Double]].convert(3)
    val c = implicitly[ConversionOpt[Int, Option[Double]]].convert(3)
    val d = implicitly[ConversionOpt[Option[Int], Option[Double]]].convert(Some(3))

    implicit def fsIdScalar[T,R](implicit s : ConversionOpt[T,R]): ConversionFuncSig[T, R] =
        new ConversionFuncSig[T, R] {
            def convert(x: T) : R = {
                s convert x
            }
        }

    implicit def fsScalarToFunction[T,R](implicit s : ConversionOpt[T,R], m : Manifest[R]): ConversionFuncSig[T, () => R] =
        new ConversionFuncSig[T, () => R]
        {
            def convert(x: T) = impl(x)

            @memo
            def impl(x : T)(implicit m : Manifest[R]) : () => R = () => s convert x
        }

    implicit def fsIdFunction[T,R](implicit s : ConversionOpt[T,R], m : Manifest[R]): ConversionFuncSig[() => T, () => R] =
        new ConversionFuncSig[() => T, () => R] {

            @memo
            def impl(x: () => T)(implicit m : Manifest[R]) : () => R = {
                () => s convert x()
            }

            def convert(x: () => T) = impl(x)
        }

    implicit def fsIdSignal[T,R](implicit s : ConversionOpt[T,R], m : Manifest[R]):
                                    ConversionFuncSig[reactive.Signal[T], reactive.Signal[R]] =
        new ConversionFuncSig[reactive.Signal[T], reactive.Signal[R]] {

            @memo
            def impl(x: reactive.Signal[T])(implicit m : Manifest[R]) : reactive.Signal[R] = {
                reactive.Unary(x,s"$x.as[Signal[$m]]"){ s.convert }
            }

            def convert(x: reactive.Signal[T]) = impl(x)
        }

    implicit def fsScalarToSignal[T,R](implicit s : ConversionOpt[T,R],
                                                m : Manifest[R]): ConversionFuncSig[T, reactive.Signal[R]] =
        new ConversionFuncSig[T, reactive.Signal[R]] {

            @memo
            def impl(x: T)(implicit m : Manifest[R]) : reactive.Signal[R] = {
                reactive.Constant(s convert x)
            }

            def convert(x: T) = impl(x)
        }

    implicit def fsSignalToFunction[T,R](implicit s : ConversionOpt[T,R],
                                                  m : Manifest[R]): ConversionFuncSig[reactive.Signal[T], () => R] =
        new ConversionFuncSig[reactive.Signal[T], () => R]
        {
            @memo
            def impl(x: reactive.Signal[T])(implicit m : Manifest[R]) : () => R = {
                () => s convert x()
            }

            def convert(x: reactive.Signal[T]) = impl(x)
        }

    val e = implicitly[ConversionFuncSig[Option[Int], Option[Double]]].convert(Some(3))
    val f = implicitly[ConversionFuncSig[() => Option[Int], () => Option[Double]]].convert(() => Some(3))

    implicit def boundId[T,R](implicit s : ConversionFuncSig[T,R]): ConversionUnbound[T, R] =
        new ConversionUnbound[T, R] {
            def convert(x: T) : R = {
                s convert x
            }
        }

    implicit def boundToUnbound[T,R](implicit s : ConversionFuncSig[T,R],
                                              m : Manifest[R]): ConversionUnbound[T, Unbound[R]] =
        new ConversionUnbound[T, Unbound[R]]
        {
            @memo
            def impl(x: T)(implicit m : Manifest[R]) : Unbound[R] = {
                (ctx : Context) => s convert x
            }
            def convert(x: T) = impl(x)
        }

    implicit def unboundId[T,R](implicit s : ConversionFuncSig[T,R],
                                         m : Manifest[R]): ConversionUnbound[Unbound[T], Unbound[R]] =
        new ConversionUnbound[Unbound[T], Unbound[R]]
        {
            @memo
            def impl(x: Unbound[T])(implicit m : Manifest[R]) : Unbound[R] = {
                (ctx : Context) => s convert x(ctx)
            }
            def convert(x: Unbound[T]) = impl(x)
        }


    val g = implicitly[ConversionUnbound[() => Option[Int], () => Option[Double]]].convert(() => Some(3))
    val h = implicitly[ConversionUnbound[() => Option[Int], Unbound[() => Option[Double]]]].convert(() => Some(3))
    val i = implicitly[ConversionUnbound[Unbound[() => Option[Int]], Unbound[() => Option[Double]]]].convert(unbound(() => Some(3)))

}