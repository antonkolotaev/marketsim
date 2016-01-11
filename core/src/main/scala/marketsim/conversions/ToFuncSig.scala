package marketsim
package conversions

import memoization.memo

trait ToFuncSig {

    @memo
    implicit def fsIdScalar[T,R](implicit s : ConversionOpt[T,R]): ConversionFuncSig[T, R] =
        new ConversionFuncSig[T, R] {
            val convert = {
                (x: T) => s convert x
            }
        }

    @memo
    implicit def fsScalarToFunction[T,R](implicit s : ConversionOpt[T,R]): ConversionFuncSig[T, () => R] =
        new ConversionFuncSig[T, () => R]
        {
            val convert = (x: T) => Compose(Const(x), s.convert)
        }

    @memo
    implicit def fsIdFunction[T,R](implicit s : ConversionOpt[T,R]): ConversionFuncSig[() => T, () => R] =
        new ConversionFuncSig[() => T, () => R] {

            val convert = (x: () => T) => Compose(x, s.convert)
        }

    @memo
    implicit def fsIdSignal[T,R](implicit s : ConversionOpt[T,R], m : Manifest[T]):
    ConversionFuncSig[reactive.Signal[T], reactive.Signal[R]] =
        new ConversionFuncSig[reactive.Signal[T], reactive.Signal[R]] {

            val convert = (x: reactive.Signal[T]) => reactive.Unary(x,s"$x.as[Signal[$m]]"){ s.convert }
        }

    @memo
    implicit def fsScalarToSignal[T,R](implicit s : ConversionOpt[T,R]): ConversionFuncSig[T, reactive.Signal[R]] =
        new ConversionFuncSig[T, reactive.Signal[R]]
        {
            val convert = (x: T) => reactive.Constant(s convert x)
        }

    @memo
    implicit def fsSignalToFunction[T,R](implicit s : ConversionOpt[T,R]): ConversionFuncSig[reactive.Signal[T], () => R] =
        new ConversionFuncSig[reactive.Signal[T], () => R]
        {
            val convert = (x: reactive.Signal[T]) => Compose(x, s.convert)
        }


}
