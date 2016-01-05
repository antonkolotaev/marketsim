package marketsim
package conversions

trait ToOption {

    implicit def idOpt[T,R](implicit s : ScalarConversion[T,R]): ConversionOpt[T, R] =
        new ConversionOpt[T, R] {
            def convert(x: T) : R = {
                s convert x
            }
        }

    implicit def toOpt[T,R](implicit s : ScalarConversion[T,R]): ConversionOpt[T, Option[R]] =
        new ConversionOpt[T, Option[R]] {
            def convert(x: T) : Option[R] = {
                Some(s convert x)
            }
        }

    implicit def betweenOpt[T,R](implicit s : ScalarConversion[T,R]): ConversionOpt[Option[T], Option[R]] =
        new ConversionOpt[Option[T], Option[R]] {
            def convert(x: Option[T]) : Option[R] = {
                x map { s.convert }
            }
        }

}
