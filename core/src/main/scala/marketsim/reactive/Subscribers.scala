package marketsim.reactive

/**
 * A set of subscribers implemented on top of a list
 * @tparam T -- subscriber type
 */
class Subscribers[T]
{
    private var listeners = List.empty[T]

    def add(listener : T) = {
        listeners find { _ == listener } match {
            case Some(x) =>
            case None => listeners = listener :: listeners
        }
    }

    def remove(listener : T) = {
        listeners = listeners filter  { _ != listener }
    }

    // TODO: these methods should be implemented using standard traits
    def foreach(action : T => Unit) {
        listeners foreach { action }
    }

    def foldLeft[A](startValue : A)(f : (A, T) => A) = {
        listeners.foldLeft(startValue)(f)
    }

    def nonEmpty = listeners.nonEmpty
}
