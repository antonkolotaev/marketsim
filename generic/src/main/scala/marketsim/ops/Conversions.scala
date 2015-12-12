package marketsim
package ops

object Conversions {

    import reactive._

    implicit def toOptionId[T]: Conversion[T, Option[T]] =
        new Conversion[T, Option[T]] {
            def convert(x: T) : Option[T] = Some(x)
        }

    implicit def toOptionId_ctx[C,T]: Conversion[C => T, C => Option[T]] =
        new Conversion[C => T, C => Option[T]] {
            def convert(x: C => T) : C => Option[T] = (c : C) => Some(x(c))
        }

    implicit def toOptionId_ctx2[C,T]: Conversion[T, C => Option[T]] =
        new Conversion[T, C => Option[T]] {
            def convert(x: T) = c => Some(x)
        }

    implicit def toSignal[T]: Conversion[T, Signal[T]] =
        new Conversion[T, Signal[T]] {
            def convert(x : T) = new Variable(x, x.toString)
        }

    implicit def toSignal_ctx[C,T]: Conversion[C => T, C => Signal[T]] =
        new Conversion[C => T, C => Signal[T]] {
            def convert(x : C => T) = c => new Variable(x(c), x(c).toString)
        }

    implicit def toOptionSignal[T]: Conversion[T, Signal[Option[T]]] =
        new Conversion[T, Signal[Option[T]]] {
            def convert(x : T) = new Variable(Some(x), x.toString)
        }

    implicit def toOptionSignal2[T]: Conversion[Signal[T], Signal[Option[T]]] =
        new Conversion[Signal[T], Signal[Option[T]]] {
            def convert(x : Signal[T]) = Unary(x,s"$x.opt") { y => Some(y) }
        }


    implicit def toFunction[T]: Conversion[T, () => T] =
        new Conversion[T, () => T] {
            def convert(x : T) = () => x
        }

    implicit def toOptionFunction[T]: Conversion[T, () => Option[T]] =
        new Conversion[T, () => Option[T]] {
            def convert(x : T) = () => Some(x)
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
