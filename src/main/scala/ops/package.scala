package object ops
{
    import language.implicitConversions

    trait Conversion[From, To]
    {
        def convert(from : From) : To
    }

    trait Conversions_Level1 {
        implicit def compose[A,B,C](implicit a2b : Conversion[A,B], b2c : Conversion[B,C]) : Conversion[A,C] =
            new Conversion[A,C] {
                def convert(a: A) = b2c.convert(a2b.convert(a))
            }
    }

    object Conversions extends Conversions_Level1 {

        implicit def id[T] : Conversion[T,T] =
            new Conversion[T,T]
            {
                def convert(x : T) = x
            }

        implicit val int2double     = new Conversion[Int,  Double]{  def convert(x : Int) = x  }
        implicit val int2long       = new Conversion[Int,  Long]  {  def convert(x : Int) = x  }
        implicit val long2double    = new Conversion[Long, Double]{  def convert(x : Long) = x }

        implicit def toOption[T, U](implicit ev : Conversion[T,U]) : Conversion[T, Option[U]] =
            new Conversion[T, Option[U]]
            {
                def convert(x : T) = Some(ev.convert(x))
            }
    }

    trait PlusDefined[A,B]
    {
        type Ret
        def plus(a : A, b : B) : Ret
    }

    trait Addable_Level1 {
        implicit def rightConversion[A,B](implicit  c : Conversion[A,B], ev : PlusDefined[B,B]) : PlusDefined[A,B] =
            new PlusDefined[A,B]
            {
                type Ret = PlusDefined[B,B]#Ret
                def plus(a : A, b : B) = ev.plus(c.convert(a), b)
            }

        implicit def leftConversion[A,B](implicit  c : Conversion[B,A], ev : PlusDefined[A,A]) : PlusDefined[A,B] =
            new PlusDefined[A,B]
            {
                type Ret = PlusDefined[A,A]#Ret
                def plus(a : A, b : B) = ev.plus(a, c.convert(b))
            }

    }

    object Addable extends Addable_Level1 {
        implicit def fromNumeric[T : Numeric] : PlusDefined[T,T] = new PlusDefined[T,T]
        {
            type Ret = T
            def plus(a : T, b : T) = implicitly[Numeric[T]].plus(a,b)
        }
    }


}

