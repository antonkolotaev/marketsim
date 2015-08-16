package reactive

/**
 * External interface of our event propagation system
 * allows to subscribe to listen value changes
 * @tparam T - type of value held by our observable
 */
trait HasExternalSubscribers[T]
{
    protected val external = new Subscribers[T => Unit]

    // registers external listener
    def += (listener : T => Unit) = {
        external add listener
    }

    // unregisters external listener
    def -= (listener : T => Unit) = {
        external remove listener
    }
}
