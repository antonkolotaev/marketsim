package object core {

    case class Time(x : Long)
    {
        def < (other : Time) = x < other.x
        def + (dt : Duration) = Time(x + dt.x)
    }

    case class Duration(x : Long)
}
