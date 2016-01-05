package marketsim
package conversions

import memoization.memo

trait ToFuncSig {

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


}
