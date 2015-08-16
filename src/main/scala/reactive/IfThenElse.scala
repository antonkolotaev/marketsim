package reactive

/**
 * Computable observable for a conditional expression
 * @param condition -- input observable of boolean type for the condition
 * @param ifBranch -- input observable for positive case
 * @param elseBranch -- input observable for negative case
 * @tparam Result -- type of values involved
 */
case class IfThenElse[Result](condition  : Value[Boolean],
                              ifBranch   : Value[Result],
                              elseBranch : Value[Result])
    extends Value[Result](if (condition()) ifBranch() else elseBranch())
{
    private var cachedCondition = condition()
    private var cachedIf        = ifBranch()
    private var cachedElse      = elseBranch()

    lazy val inputs = condition :: ifBranch :: elseBranch :: Nil

    finalConstruct()

    /**
     * Makes the current value of the observable consistent with the input values
     */
    def validate() = {
        // if condition changed
        if (cachedCondition != condition())
            // cache it
            cachedCondition = condition()

        // if positive branch active and has changed
        if (cachedCondition && ifBranch() != cachedIf)
            // cache it
            cachedIf = ifBranch()

        // if negative branch active and has changed
        if (!cachedCondition && elseBranch() != cachedElse)
            // cache it
            cachedElse = elseBranch()

        // update current value and if changed notify all dependent observables and listeners
        updateValue(if (cachedCondition) cachedIf else cachedElse)
    }

    override def toString() = s"if $condition then $ifBranch else $elseBranch"
}
