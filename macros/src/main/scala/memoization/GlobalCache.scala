package memoization

object GlobalCache
{
    class MethodCache[V] extends Cache[V]
    {
        private val cache = collection.mutable.Map.empty[List[Any], V]

        def get(key : List[Any], compute : => V) =
            cache getOrElseUpdate (key, compute)

        override def toString = cache.keys map { _ mkString ("{",",","}") } mkString ("[", ",", "]")
    }

    object Builder extends MemoCacheBuilder {

        private val caches = collection.mutable.Map.empty[String, MethodCache[_]]

        override def build[V <: Object](bucketId: String): Cache[V] =
            (caches getOrElseUpdate (bucketId, new MethodCache[V])).asInstanceOf[MethodCache[V]]

        override def toString = caches map { case (k,v) => s"$k : $v" } mkString "\n"
    }


}
