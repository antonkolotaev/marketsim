package marketsim
package ops

import memoization.memo

object Casts {


    import reactive._

    implicit def t_ut[T]: Conversion[T, Unbound[T]] =
        new Conversion[T, Unbound[T]] {
            @memo def convert(x: T) : Unbound[T] = {
                unbound(x)
            }
        }

    implicit def t_ot[T]: Conversion[T, Option[T]] =
        new Conversion[T, Option[T]] {
            @memo def convert(x: T) : Option[T] = {
                Some(x)
            }
        }

    implicit def ut_uot[C,T]: Conversion[Unbound[T], Unbound[Option[T]]] =
        new Conversion[Unbound[T], Unbound[Option[T]]] {
            @memo def convert(x: Unbound[T]) : Unbound[Option[T]] = {
                (c : Context) => Some(x(c))
            }
        }

    implicit def t_uot[C,T]: Conversion[T, Unbound[Option[T]]] =
        new Conversion[T, Unbound[Option[T]]] {
            @memo def convert(x: T) : Unbound[Option[T]] = {
                unbound(Some(x) : Option[T])
            }
        }

    implicit def t_st[T]: Conversion[T, Signal[T]] =
        new Conversion[T, Signal[T]] {
            @memo def convert(x : T) : Signal[T] = Constant(x)
        }

    implicit def ut_ust[C,T]: Conversion[Unbound[T], Unbound[Signal[T]]] =
        new Conversion[Unbound[T], Unbound[Signal[T]]] {
            @memo def convert(x : Unbound[T]) : Unbound[Signal[T]] = {
                (c : Context) => Constant(x(c))
            }
        }

    implicit def t_ust[C,T]: Conversion[T, Unbound[Signal[T]]] =
        new Conversion[T, Unbound[Signal[T]]] {
            @memo def convert(x : T) : Unbound[Signal[T]] = {
                unbound(Constant(x))
            }
        }

    implicit def t_sot[T]: Conversion[T, Signal[Option[T]]] =
        new Conversion[T, Signal[Option[T]]] {
            @memo def convert(x : T) : Signal[Option[T]] = Constant(Some(x))
        }

    implicit def t_usot[T]: Conversion[T, Unbound[Signal[Option[T]]]] =
        new Conversion[T, Unbound[Signal[Option[T]]]] {
            @memo def convert(x : T) : Unbound[Signal[Option[T]]] = unbound(Constant(Some(x)))
        }

    implicit def ut_usot[T]: Conversion[Unbound[T], Unbound[Signal[Option[T]]]] =
        new Conversion[Unbound[T], Unbound[Signal[Option[T]]]] {
            @memo def convert(x : Unbound[T]) : Unbound[Signal[Option[T]]] = (c : Context) => Constant(Some(x(c)))
        }

    implicit def st_sot[T]: Conversion[Signal[T], Signal[Option[T]]] =
        new Conversion[Signal[T], Signal[Option[T]]] {
            @memo def convert(x : Signal[T]) : Signal[Option[T]] = Unary(x,s"$x.opt") { y => Some(y) }
        }


    implicit def t_ft[T]: Conversion[T, () => T] =
        new Conversion[T, () => T] {
            @memo def convert(x : T) : () => T = () => x
        }

    implicit def t_uft[T]: Conversion[T, Unbound[() => T]] =
        new Conversion[T, Unbound[() => T]] {
            @memo def convert(x : T) : Unbound[() => T] = unbound(() => x)
        }

    implicit def ut_uft[T]: Conversion[Unbound[T], Unbound[() => T]] =
        new Conversion[Unbound[T], Unbound[() => T]] {
            @memo def convert(x : Unbound[T]) : Unbound[() => T] = (c : Context) => () => x(c)
        }

    implicit def t_fot[T]: Conversion[T, () => Option[T]] =
        new Conversion[T, () => Option[T]] {
            @memo def convert(x : T) : () => Option[T] = () => Some(x)
        }

    implicit def toOptionFunction2[T]: Conversion[() => T, () => Option[T]] =
        new Conversion[() => T, () => Option[T]] {
            def convert(x : () => T) = () => Some(x())
        }

    implicit def toFunction_sig[T]: Conversion[Signal[T], () => T] =
        new Conversion[Signal[T], () => T] {
            def convert(x : Signal[T]) = x
        }

    implicit def toOptionFunction_sig[T]: Conversion[Signal[T], () => Option[T]] =
        new Conversion[Signal[T], () => Option[T]] {
            def convert(x : Signal[T]) = () => Some(x())
        }


}
