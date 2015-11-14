package reactive

/**
 * A reactive signal (or simply signal) is a value that changes discretely
 * Users of this class may subscribe to its value change notifications
 * There are primary signals that are updated on external events
 * and dependent signals that functionally depend on other signals and update their value once input signal value changes
 * Primary signals should use Variable derived class
 * Base classes for dependent signals are Unary, Binary and IfThenElse
 * @param value_ -- initial value
 * @tparam T - type of value held by our observable
 */
abstract class Signal[T](protected var value_ : T)
    extends (() => T) 
    with HasInternalSubscribers 
    with HasExternalSubscribers[T]
{
    /**
     * Retrieves the signal value and if it is changed notifies listeners about it
     * Doesn't cause recalculation of the dependent signals
     * @return signal value
     * NB: we have to notify listeners eagerly since otherwise we would have to cache signal updates somewhere to notify them properly
     */
    def apply(notifyExternal : Boolean) : T = {
        if (dirty) {
            validateAndNotifyExternal(notifyExternal)
        }
        value_
    }

    def apply() : T = apply(notifyExternal = false)

    /**
     * Makes observable value consistent with values of input values
     * Every derived class defines it in its own way
     */
    protected def validate(notifyExternal : Boolean)

    private def validateAndNotifyExternal(notifyExternal : Boolean): Unit = {
        val oldValue = value_
        validate(notifyExternal)
        if (dirty || extrenalsAreToBeNotified) {
            // we need this check since validate() can call our code again
            dirty = false
            //println(s"notifyExternalListeners $this: $oldValue -> $value_")
            if (oldValue != value_ || extrenalsAreToBeNotified) {
                if (!notifyExternal)
                    extrenalsAreToBeNotified = true
                else {
                    external foreach {
                        _ apply value_
                    }
                    extrenalsAreToBeNotified = false
                }
            }
        }
    }

    /**
     * Recalculates the value if needed and if it has changed notifies our external listeners about it
     * and also tells to dependent signals to notify their listeners with their new values
     */
    protected[reactive] def notifyExternalListenersIfValueChanged() : Unit =
    {
        if (dirty) {
            if (external.nonEmpty) {
                validateAndNotifyExternal(notifyExternal = true)
            }
            //println(s"notifyExternalListeners $this: notifyDependent begin")
            internal foreach { _ notifyExternalListenersIfValueChanged () }
            //println(s"notifyExternalListeners $this: notifyDependent end")
        }
        else
        {}//println(s"notifyExternalListeners $this: dirty is already false")

    }


    /**
     * iff dirty the current value_ is suspected to be outdated and needs to be recalculated
     */
    protected var dirty = false

    private var extrenalsAreToBeNotified = false

    /**
     * Replaces the current value by 'x'.
     * This method is called by derived classes once a new value has been calculated.
     * @return true iff this call really updates the value
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
     * Marks the node as dirty and all dependent signals too
     */
    protected def invalidate() : Unit = {
        if (!dirty) {
            //println(s"invalidate $this")
            dirty = true
            internal foreach { _ invalidate () }
        }
    }
}
