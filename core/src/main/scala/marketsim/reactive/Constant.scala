package marketsim.reactive

case class Constant[T](x : T) extends Signal[T](x)
{
    def inputs = Nil

    def validate(notifyExternal : Boolean) {}

    override def toString() = x.toString
}
