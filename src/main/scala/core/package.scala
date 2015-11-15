package object core {

    case class Time(x : Long)
    {
        def < (other : Time) = x < other.x
        def <= (other : Time) = x <= other.x
        def + (dt : Duration) = Time(x + dt.x)

        override def toString = s"${x}s"
    }

    case class Duration(x : Long)
    {
        def + (other : Duration) = Duration(x + other.x)

        override def toString = s"${x}s"
    }
}
