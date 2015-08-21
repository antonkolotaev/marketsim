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
        // register itself in its inputs
        inputs foreach { _.internal add this }
    }

    private var disposed = false

    /**
     * This method should be called when a node is removed from dependent observables graph
     */
    def dispose() : Unit = {

        if (!disposed)
        {
            // first we dispose all dependent nodes
            internal foreach { _ dispose () }

            // and also from its inputs
            inputs foreach { _.internal remove this }

            disposed = true
        }
    }
}
