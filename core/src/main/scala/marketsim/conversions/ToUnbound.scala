package marketsim
package conversions

import memoization.memo

trait ToUnbound {

    @memo
    implicit def boundId[T,R](implicit s : ConversionFuncSig[T,R]): ConversionUnbound[T, R] =
        new ConversionUnbound[T, R] {
            val convert = (x: T) => {
                s convert x
            }
        }

    @memo
    implicit def boundToUnbound[T,R](implicit s : ConversionFuncSig[T,R],
                                              m : Manifest[R]): ConversionUnbound[T, Unbound[R]] =
        new ConversionUnbound[T, Unbound[R]]
        {
            val convert = Compose1(s.convert, unbound[R])
        }

    @memo
    implicit def unboundId[T,R](implicit s : ConversionFuncSig[T,R],
                                         m : Manifest[R]): ConversionUnbound[Unbound[T], Unbound[R]] =
        new ConversionUnbound[Unbound[T], Unbound[R]]
        {
            @memo
            def impl(x: Unbound[T])(implicit m : Manifest[R]) : Unbound[R] = {
                (ctx : Context) => s convert x(ctx)
            }
            val convert = (x: Unbound[T]) => impl(x)
        }
}
