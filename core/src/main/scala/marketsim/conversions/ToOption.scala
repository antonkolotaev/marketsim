package marketsim
package conversions

trait ToOption {

    implicit def idOpt[T,R](implicit s : ScalarConversion[T,R]): ConversionOpt[T, R] =
        new ConversionOpt[T, R] {
            val convert: T => R = {
                (x: T) => s convert x
            }
        }

    implicit def toOpt[T,R](implicit s : ScalarConversion[T,R]): ConversionOpt[T, Option[R]] =
        new ConversionOpt[T, Option[R]] {
            val convert: T => Option[R] = {
                (x: T) => Some(s convert x)
            }
        }

    implicit def betweenOpt[T,R](implicit s : ScalarConversion[T,R]): ConversionOpt[Option[T], Option[R]] =
        new ConversionOpt[Option[T], Option[R]] {
            val convert: Option[T] => Option[R] = {
                (x: Option[T]) => x map { s.convert }
            }
        }

}
