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
        // and in roots where it can be reached from
        registerInRoot(this, collection.mutable.Set.empty[Value[_]])
    }

    /**
     * This method should be called when a node is removed from dependent observables graph
     */
    def dispose() = {
        // remove itself from its inputs
        inputs foreach { _.internal remove this }
        // and from roots where it can be reached from
        removeFromRoot(this, collection.mutable.Set.empty[Value[_]])

        // TODO: we need to dispose also all reachable nodes
    }

    /**
     * Registers 'what' in roots where we can be accessed from
     * @param what -- node to register
     * @param visited -- a collection of already visited nodes
     */
    protected def registerInRoot(what    : Value[_],
                                 visited : collection.mutable.Set[Value[_]]) : Unit = {
        if (!(visited contains this)) {
            visited += this
            inputs foreach { _ registerInRoot(what, visited) }
        }
    }

    /**
     * Removes 'what' from roots where it can be accessed from
     * @param what -- node to remove
     * @param visited -- a collection of already visited nodes
     */
    protected def removeFromRoot(what    : Value[_],
                                 visited : collection.mutable.Set[Value[_]]) : Unit = {
        if (!(visited contains this)) {
            visited += this
            inputs foreach { _ removeFromRoot (what, visited) }
        }
    }
}
