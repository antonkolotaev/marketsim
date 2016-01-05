package marketsim


object OptionConversions
{

/*    trait Id {
        implicit def id[T,R](implicit s : ScalarConversion[T,R]) : OptionConversion[T, R] =
        {
            new OptionConversion[T, R] {
                def convert(x : T) : R = s convert x
            }
        }
    }

    trait ToOption extends Id {
        implicit def toOption[T,R](implicit s : ScalarConversion[T,R]) : OptionConversion[T, Option[R]] =
        {
            new OptionConversion[T, Option[R]] {
                def convert(x : T) : Option[R] = Some(s convert x)
            }
        }
    }

    trait BetweenOptions extends ToOption {
        implicit def betweenOptions[T,R](implicit s : ScalarConversion[T,R]) : OptionConversion[Option[T], Option[R]] =
        {
            new OptionConversion[Option[T], Option[R]] {
                def convert(x : Option[T]) : Option[R] = x map { s.convert }
            }
        }
    }

    implicit def toOption[T,R](implicit s : ScalarConversion[T,R]) : OptionConversion[T, Option[R]] =
    {
        new OptionConversion[T, Option[R]] {
            def convert(x : T) : Option[R] = Some(s convert x)
        }
    }
*/

}

object usage {
    import ScalarConversion._

    val s1 = implicitly[ScalarConversion[Int, Double]]
    //val c1 = implicitly[OptionConversion[Int, Option[Double]]]
}