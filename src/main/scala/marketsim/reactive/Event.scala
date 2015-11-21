package marketsim.reactive

class Event[T] extends HasExternalSubscribers[T]
{
    def fire (x : T) = external foreach { _ apply x }
}
