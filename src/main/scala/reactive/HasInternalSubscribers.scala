package reactive

/**
 * Auxiliary trait maintaining dependent observable graph structure
 */
trait HasInternalSubscribers
{
    // all dependent values are derived from Value[T]
    self : Value[_] =>

    /**
     * Observables dependent on this value
     */
    protected val internal = new Subscribers[Value[_]]

    /**
     * list of input observables for this observable
     */
    val inputs : List[Value[_]]

    /**
     * This method is called by descendants to update outgoing links
     */
    protected def finalConstruct() = {
        inputs foreach { _.internal add this }
    }
}
