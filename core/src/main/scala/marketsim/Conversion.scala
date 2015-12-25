package marketsim

import memoization.memo

import language.implicitConversions

trait Conversion[-From, To] {
    def convert(from: From): To
}

object Conversion {

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
                (c : Context) => t_ot.convert(x(c))
            }
        }

    implicit def t_uot[C,T]: Conversion[T, Unbound[Option[T]]] =
        new Conversion[T, Unbound[Option[T]]] {
            @memo def convert(x: T) : Unbound[Option[T]] = {
                unbound(Some(x) : Option[T])
            }
        }

    implicit def ot_sot[T]: Conversion[Option[T], Signal[Option[T]]] =
        new Conversion[Option[T], Signal[Option[T]]] {
            @memo def convert(x : Option[T]) : Signal[Option[T]] = Constant(x)
        }

    implicit def t_st[T]: Conversion[T, Signal[T]] =
        new Conversion[T, Signal[T]] {
            @memo def convert(x : T) : Signal[T] = Constant(x)
        }

    implicit def ut_ust[C,T]: Conversion[Unbound[T], Unbound[Signal[T]]] =
        new Conversion[Unbound[T], Unbound[Signal[T]]] {
            @memo def convert(x : Unbound[T]) : Unbound[Signal[T]] = {
                (c : Context) => t_st.convert(x(c))
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
            @memo def convert(x : Unbound[T]) : Unbound[Signal[Option[T]]] = (c : Context) => t_sot.convert(x(c))
        }

    implicit def st_sot[T]: Conversion[Signal[T], Signal[Option[T]]] =
        new Conversion[Signal[T], Signal[Option[T]]] {
            @memo def convert(x : Signal[T]) : Signal[Option[T]] = Unary(x,s"$x.opt") { y => Some(y) }
        }

    implicit def st_usot[T]: Conversion[Signal[T], Unbound[Signal[Option[T]]]] =
        new Conversion[Signal[T], Unbound[Signal[Option[T]]]] {
            @memo def convert(x : Signal[T]) : Unbound[Signal[Option[T]]] = {
                (c : Context) => st_sot.convert(x)
            }
        }

    implicit def ust_usot[T]: Conversion[Unbound[Signal[T]], Unbound[Signal[Option[T]]]] =
        new Conversion[Unbound[Signal[T]], Unbound[Signal[Option[T]]]] {
            @memo def convert(x : Unbound[Signal[T]]) : Unbound[Signal[Option[T]]] =
                (c : Context) => st_sot.convert(x(c))
        }

    implicit def ust_ufot[T]: Conversion[Unbound[Signal[T]], Unbound[() => Option[T]]] =
        new Conversion[Unbound[Signal[T]], Unbound[() => Option[T]]] {
            @memo def convert(x : Unbound[Signal[T]]) : Unbound[() => Option[T]] =
                (c : Context) => st_fot.convert(x(c))
        }

    implicit def ust_uft[T]: Conversion[Unbound[Signal[T]], Unbound[() => T]] =
        new Conversion[Unbound[Signal[T]], Unbound[() => T]] {
            @memo def convert(x : Unbound[Signal[T]]) : Unbound[() => T] =
                (c : Context) => st_ft.convert(x(c))
        }

    implicit def t_ft[T]: Conversion[T, () => T] =
        new Conversion[T, () => T] {
            @memo def convert(x : T) : () => T = () => x
        }

    implicit def t_uft[T]: Conversion[T, Unbound[() => T]] =
        new Conversion[T, Unbound[() => T]] {
            @memo def convert(x : T) : Unbound[() => T] = unbound(() => x)
        }

    implicit def t_ufot[T]: Conversion[T, Unbound[() => Option[T]]] =
        new Conversion[T, Unbound[() => Option[T]]] {
            @memo def convert(x : T) : Unbound[() => Option[T]] = unbound(() => Some(x))
        }

    implicit def ut_uft[T]: Conversion[Unbound[T], Unbound[() => T]] =
        new Conversion[Unbound[T], Unbound[() => T]] {
            @memo def convert(x : Unbound[T]) : Unbound[() => T] = (c : Context) => t_ft.convert(x(c))
        }

    implicit def t_fot[T]: Conversion[T, () => Option[T]] =
        new Conversion[T, () => Option[T]] {
            @memo def convert(x : T) : () => Option[T] = () => Some(x)
        }

    implicit def ft_fot[T]: Conversion[() => T, () => Option[T]] =
        new Conversion[() => T, () => Option[T]] {
            @memo def convert(x : () => T) : () => Option[T] = () => Some(x())
        }

    implicit def ft_ufot[T]: Conversion[() => T, Unbound[() => Option[T]]] =
        new Conversion[() => T, Unbound[() => Option[T]]] {
            @memo def convert(x : () => T) : Unbound[() => Option[T]] = unbound(() => Some(x()))
        }

    implicit def uft_ufot[T]: Conversion[Unbound[() => T], Unbound[() => Option[T]]] =
        new Conversion[Unbound[() => T], Unbound[() => Option[T]]] {
            @memo def convert(x : Unbound[() => T]) : Unbound[() => Option[T]] = (c : Context) => ft_fot.convert(x(c))
        }

    implicit def st_ft[T]: Conversion[Signal[T], () => T] =
        new Conversion[Signal[T], () => T] {
            @memo def convert(x : Signal[T]) : () => T = x
        }

    implicit def st_fot[T]: Conversion[Signal[T], () => Option[T]] =
        new Conversion[Signal[T], () => Option[T]] {
            @memo def convert(x : Signal[T]) : () => Option[T] = () => Some(x())
        }

    implicit def st_ufot[T]: Conversion[Signal[T], Unbound[() => Option[T]]] =
        new Conversion[Signal[T], Unbound[() => Option[T]]] {
            @memo def convert(x : Signal[T]) : Unbound[() => Option[T]] = unbound(() => Some(x()))
        }

    implicit def ut_ufot[T]: Conversion[Unbound[T], Unbound[() => Option[T]]] =
        new Conversion[Unbound[T], Unbound[() => Option[T]]] {
            @memo def convert(x : Unbound[T]) : Unbound[() => Option[T]] = (c : Context) => t_fot.convert(x(c))
        }

    implicit class Rich[T](x : T)
    {
        def as[R](implicit c : Conversion[T, R]) = c convert x
    }

}