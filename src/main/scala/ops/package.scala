package object ops
{

    trait Conversion[From, To]
    {
        def convert(from : From) : To
    }

    object Conversions {

        implicit def id[T] : Conversion[T,T] =
            new Conversion[T,T]
            {
                def convert(x : T) = x
            }

        implicit val int2double = new Conversion[Int, Double]{  def convert(x : Int) = x  }
        implicit val int2long = new Conversion[Int, Long]{  def convert(x : Int) = x  }
        implicit val long2double = new Conversion[Long, Double]{  def convert(x : Long) = x  }

        implicit def toOption[T, U](implicit ev : Conversion[T,U]) : Conversion[T, Option[U]] =
            new Conversion[T, Option[U]]
            {
                def convert(x : T) = Some(ev.convert(x))
            }

        /* TODO: understand why it doesn't work
        implicit def compose[A,B,C](implicit a2b : ConversionDefined[A,B], b2c : ConversionDefined[B,C]) : ConversionDefined[A,C] =
            new ConversionDefined[A,C]
            {
                def convert(a : A) = b2c.convert(a2b.convert(a))
            }
            */
    }

    trait PlusDefined[A,B]
    {
        type Ret
        def plus(a : A, b : B) : Ret
    }

    object Addable {
        implicit def fromNumeric[T : Numeric] : PlusDefined[T,T] = new PlusDefined[T,T]
        {
            type Ret = T
            def plus(a : T, b : T) = implicitly[Numeric[T]].plus(a,b)
        }

        //implicit def rightConversion[A,B : PlusDefined](implicit  ev : )
    }


}

