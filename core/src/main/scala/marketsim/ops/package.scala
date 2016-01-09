package marketsim

package object ops {

    def IsSome[T](x : reactive.Signal[Option[T]])(implicit m : Manifest[T]) = reactive.Unary(x, s"$x.IsSome") { _.nonEmpty }
    def GetSome[T](x : reactive.Signal[Option[T]])(implicit m : Manifest[T]) = reactive.Unary(x, s"$x.IsSome") { _.get }

}
