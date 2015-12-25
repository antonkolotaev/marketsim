package marketsim

object Cached
{
    private val threadName_ = new util.DynamicVariable(Thread.currentThread().getName)

    def withThreadName[T](s : String)(block : => T) = threadName_.withValue(s) (block)
    def threadName = threadName_.value

    private type CacheType = collection.mutable.HashMap[(Manifest[_], String, Any), Any]
    private val cache_ = new util.DynamicVariable(Option.empty[CacheType])
    private def cache = cache_.value.get

    def withNew[T](x : => T) = {
        cache_.value map { _ => x } getOrElse cache_.withValue(Some(new CacheType)) {
            x
        }
    }

    def put[R](key : Any, value : => R, s : String)(implicit m : Manifest[R]) = {
        cache update ((m,s,key), value)
    }

    def get[R](key : Any, s : String)(implicit m : Manifest[R]) = {
        cache get ((m,s,key)) map {_.asInstanceOf[R]}
    }

    def getOrElseUpdate[R](key : Any, value : => R, s : String)(implicit m : Manifest[R]) = {
        (cache getOrElseUpdate ((m,s,key), value)).asInstanceOf[R]
    }

    override def toString = (cache map { case ((m, s, k),v) => s"$s: $k (${k.hashCode()}) => $v ${v.hashCode()} | Manifest = $m"}).toList.sorted mkString ("[", ",\n", "]")

}
