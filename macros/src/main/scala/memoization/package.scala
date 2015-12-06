package object memoization {

    /**
     * Based on MacMemo https://github.com/kciesielski/macmemo
     */


    trait Cache[V] {

        /**
         * Return cached value for given key (method's parameters).
         * If needed, computeValue will be called to obtain it.
         *
         * @param key method argument values
         * @param computeValue a non-strict loader for a value.
         * @return cached value
         */
        def get(key: List[Any], computeValue: => V): V

    }

    trait MemoCacheBuilder {

        def build[V <: Object](bucketId: String): Cache[V]

    }


}
