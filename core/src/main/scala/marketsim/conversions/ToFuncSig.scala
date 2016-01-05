package marketsim
package conversions

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
            def convert(x: T) = Compose(Const(x), s.convert)
        }

    implicit def fsIdFunction[T,R](implicit s : ConversionOpt[T,R], m : Manifest[R]): ConversionFuncSig[() => T, () => R] =
        new ConversionFuncSig[() => T, () => R] {

            def convert(x: () => T) = Compose(x, s.convert)
        }

    implicit def fsIdSignal[T,R](implicit s : ConversionOpt[T,R], m : Manifest[R]):
    ConversionFuncSig[reactive.Signal[T], reactive.Signal[R]] =
        new ConversionFuncSig[reactive.Signal[T], reactive.Signal[R]] {

            def convert(x: reactive.Signal[T]) = reactive.Unary(x,s"$x.as[Signal[$m]]"){ s.convert }
        }

    implicit def fsScalarToSignal[T,R](implicit s : ConversionOpt[T,R],
                                       m : Manifest[R]): ConversionFuncSig[T, reactive.Signal[R]] =
        new ConversionFuncSig[T, reactive.Signal[R]] {

            def convert(x: T) = reactive.Constant(s convert x)
        }

    implicit def fsSignalToFunction[T,R](implicit s : ConversionOpt[T,R],
                                         m : Manifest[R]): ConversionFuncSig[reactive.Signal[T], () => R] =
        new ConversionFuncSig[reactive.Signal[T], () => R]
        {
            def convert(x: reactive.Signal[T]) = Compose(x, s.convert)
        }


}
