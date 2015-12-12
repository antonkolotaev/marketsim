package marketsim
package ops

trait IntBased[T]
{
    def plus(x : T, y : T) : T
    def minus(x : T, y : T) : T
    def mul(x : T, y : Int) : T
    def div_floor(x : T, y : Int) : T
    def neg(x : T) : T
    def zero : T
}

trait HasConversion[From, To]
{
    def convert(x : From) : To
}

object overloads
{
    import reactive._

    def const[T](x : T) = new Variable[T](x, x.toString)

    implicit val IntIsIntBased = new IntBased[Int] {
        def plus(x : Int, y : Int) = x + y
        def minus(x : Int, y : Int) = x - y
        def mul(x : Int, y : Int) = x * y
        def div_floor(x : Int, y : Int) = x / y
        def neg(x : Int) = -x
        def zero = 0
    }

    class ConversionToOption[T] extends HasConversion[T, Option[T]]
    {
        def convert(x : T) = Some(x)
    }

    class ConversionToSignal[T] extends HasConversion[T, reactive.Signal[T]]
    {
        def convert(x : T) = new Variable[T](x, x.toString)
    }

    implicit def hasToOption[T]: ConversionToOption[T] = new ConversionToOption[T]
    implicit def hasToSignal[T]: ConversionToSignal[T] = new ConversionToSignal[T]

    implicit def toOption[T](x : T): Option[T] = Some(x)
    implicit def toSignal[T](x : T): Signal[T] = new Variable(x, x.toString)

    implicit class RichIntBased[T](x : T)(implicit ev : IntBased[T])
    {
        def + (y : T) =
            ev.plus(x,y)
        def + (y : Option[T]) =
            y map { z => ev.plus(x,z) }
        def + (y : reactive.Signal[T]) =
            reactive.Binary(const(x), y, "+") { case (a,b) => ev.plus(a,b) }
        def + (y : reactive.Signal[Option[T]])(implicit d : Manifest[T]) =
            reactive.Binary(const(x), y, "+") { case (a,b) => b map { z => ev.plus(a,z) } }
        def + (y : () => T) =
            () => { ev.plus(x, y()) }
        def + (y : () => Option[T])(implicit d : Manifest[T]) =
            () => { y() map { z => ev.plus(x,z)} }
        

    }

    implicit class RichOptionIntBased[T](x : Option[T])(implicit ev : IntBased[T])
    {
        def - (y : Option[T]) =
            (x, y) match {
                case (Some(a), Some(b)) => Some(ev.minus(a,b))
                case _ => None
            }
    }
}

