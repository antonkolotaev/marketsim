package object ops
{
    import language.implicitConversions

    trait Conversion[From, To]
    {
        def convert(from : From) : To
    }

    object Conversions {

        implicit def toOption[T, U](implicit ev : Conversion[T,U]) : Conversion[T, Option[U]] =
            new Conversion[T, Option[U]]
            {
                def convert(x : T) = Some(ev.convert(x))
            }
        implicit def toOptionId[T] : Conversion[T, Option[T]] =
            new Conversion[T, Option[T]]
            {
                def convert(x : T) = Some(x)
            }
        implicit def betweenOptions[T, U](implicit ev : Conversion[T,U]) : Conversion[Option[T], Option[U]] =
            new Conversion[Option[T], Option[U]]
            {
                def convert(x : Option[T]) = x map { y => ev.convert(y) }
            }

        implicit val int2double     = new Conversion[Int,  Double]{  def convert(x : Int) = x  }
        implicit val int2long       = new Conversion[Int,  Long]  {  def convert(x : Int) = x  }
        implicit val long2double    = new Conversion[Long, Double]{  def convert(x : Long) = x }

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

    trait Addable_Level2  extends Addable_Level1
    {
        implicit def rightOpt[A,B](implicit  ev : PlusDefined[A,B]) : PlusDefined[A,Option[B]] =
            new PlusDefined[A,Option[B]]
            {
                type Ret = Option[PlusDefined[A,B]#Ret]
                def plus(a : A, b : Option[B]) = b map { x =>  ev.plus(a,x) }
            }
        implicit def leftOpt[A,B](implicit  ev : PlusDefined[A,B]) : PlusDefined[Option[A],B] =
            new PlusDefined[Option[A],B]
            {
                type Ret = Option[PlusDefined[A,B]#Ret]
                def plus(a : Option[A], b : B) = a map { x =>  ev.plus(x,b) }
            }
        implicit def bothOpt[A,B](implicit  ev : PlusDefined[A,B]) : PlusDefined[Option[A],Option[B]] =
            new PlusDefined[Option[A],Option[B]]
            {
                type Ret = Option[PlusDefined[A,B]#Ret]
                def plus(a : Option[A], b : Option[B]) =
                    (a,b) match {
                        case (Some(x), Some(y)) => Some(ev.plus(x,y))
                        case _ => None
                    }
            }
    }

    object Addable extends Addable_Level2 {
        implicit def fromNumeric[T : Numeric] : PlusDefined[T,T] = new PlusDefined[T,T]
        {
            type Ret = T
            def plus(a : T, b : T) = implicitly[Numeric[T]].plus(a,b)
        }
    }

    def and[A,B](a : reactive.Value[A], b : reactive.Value[B]) = reactive.Binary(a,b) { case (x,y) => (x,y) }
}

