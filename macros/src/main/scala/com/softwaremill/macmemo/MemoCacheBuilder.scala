package com.softwaremill.macmemo

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

trait MemoCacheBuilder2 {

  def build[V <: Object](bucketId: String): Cache[V]

}

object GlobalCache
{
  class MethodCache[V] extends Cache[V]
  {
    private val cache = collection.mutable.Map.empty[List[Any], V]

    def get(key : List[Any], compute : => V) =
      cache getOrElseUpdate (key, compute)

    override def toString = cache.keys map { _ mkString ("{",",","}") } mkString ("[", ",", "]")
  }

  object Builder extends MemoCacheBuilder2 {

    private val caches = collection.mutable.Map.empty[String, MethodCache[_]]

    override def build[V <: Object](bucketId: String): Cache[V] =
      (caches getOrElseUpdate (bucketId, new MethodCache[V])).asInstanceOf[MethodCache[V]]

    override def toString = caches map { case (k,v) => s"$k : $v" } mkString "\n"
  }


}
