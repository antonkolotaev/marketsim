package reactive

/**
 * Base class for dependent observables graph nodes: Variables, Unary, Binary and IfThenElse
 * Represents a value that can be invalidated and allows to subscribe other invalidables
 * @param value_ -- initial value
 * @tparam T - type of value held by our observable
 */
abstract class Value[T](protected var value_ : T)
    extends (() => T) 
    with HasInternalSubscribers 
    with HasExternalSubscribers[T]
{
    /**
     * iff dirty the current value_ is suspected to be outdated and needs to be recalculated
     */
    protected var dirty = false

    /**
     * Replaces the current value by 'x'.
     * This method is called by derived classes 
     * once a new value has been calculated.
     * If it differs from the existing one it is updated 
     * and dependent observables are invalidated
     * @return true iff the value was really updated
     */
    protected def updateValue(x : T) = {
        if (value_ != x)
        {
            //println(s"updating $this by $x")
            value_ = x
            true
        }
        else false
    }

    /**
     * Marks the node as dirty and all dependent observables too
     */
    def invalidate() : Unit = {
        if (!dirty) {
            //println(s"invalidate $this")
            dirty = true
            internal foreach { _ invalidate () }
        }
    }

    /**
     * Recalculates the value if needed and if it has changed notifies our external listeners about it
     * and also tells to dependent observables to notify their listeners with their new values
     */
    def notifyExternalListenersIfValueChanged() : Unit =
    {
        if (dirty) {
            if (external.nonEmpty) {
                validateAndNotifyExternal()
            }
            //println(s"notifyExternalListeners $this: notifyDependent begin")
            internal foreach { _ notifyExternalListenersIfValueChanged () }
            //println(s"notifyExternalListeners $this: notifyDependent end")
        }
        else
            {}//println(s"notifyExternalListeners $this: dirty is already false")

    }

    private def validateAndNotifyExternal(): Unit = {
        val oldValue = value_
        validate()
        if (dirty) {
            // we need this check since validate() can call our code again
            dirty = false
            //println(s"notifyExternalListeners $this: $oldValue -> $value_")
            if (oldValue != value_) {
                external foreach {
                    _ apply value_
                }
            }
        }
    }

    /**
     * Makes observable value consistent with values of input values
     * Every derived class defines it in its own way
     */
    protected def validate()

    /**
     * Retrieves the value.
     * Side effect: if a value changed its listeners are notified
     * @return
     */
    def apply() = {
        //println(s"$this.apply")
        if (dirty) {
            validateAndNotifyExternal()
            //println(s"$this.apply: notifyDependent begin")
            internal foreach { _ notifyExternalListenersIfValueChanged () }
            //println(s"$this.apply: notifyDependent end")
        }
        else
            {}//println(s"$this.apply: dirty is already false")
        value_
    }
}
