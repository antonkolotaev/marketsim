package reactive

/**
 * Auxiliary trait maintaining dependent observable graph structure
 */
trait HasInternalSubscribers
{
    // all dependent values are derived from Value[T]
    self : Signal[_] =>

    /**
     * Observables dependent on this value
     */
    protected val internal = new Subscribers[Signal[_]]

    /**
     * list of input observables for this observable
     */
    def inputs : List[Signal[_]]

    /**
     * This method is called by descendants to update outgoing links
     */
    protected def finalConstruct() = {
        // register itself in its inputs
        inputs foreach { _.internal add this }
    }

    private var disposed_ = false

    def disposed = disposed_

    /**
     * This method should be called when a node is removed from dependent observables graph
     */
    def dispose() : Unit = {

        if (!disposed_)
        {
            // first we dispose all dependent nodes
            internal foreach { _ dispose () }

            // and also from its inputs
            inputs foreach { _.internal remove this }

            disposed_ = true
        }
    }
}
