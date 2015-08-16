package object computable
{
  private var globalUpdateId = 0

  trait HasInternalSubscribers extends Disposable
  {
    self : Value[_] =>

    protected val internal = new Subscribers[Value[_]]

    // list of input observables for this observable
    val inputs : List[Value[_]]

    def dispose() = {
      inputs foreach { _.internal remove this }
    }

    def finalConstruct() = {
      inputs foreach { _.internal add this }
    }
  }

  // External interface of our event propagation system
  // allows to subscribe to listen value changes
  trait HasExternalSubscribers[T]
  {
    protected val external = new Subscribers[T => Unit]

    // registers external listener
    def += (listener : T => Unit) = {
      external add listener
    }

    // unregisters external listener
    def -= (listener : T => Unit) = {
      external remove listener
    }
  }

  trait Disposable {
    def dispose()
  }

  // normally all classes derive from this one also
  // reason to extract Invalidable from Value[T]
  // is to get rid of type parameter where it is not needed
  //
  // Represents a value that can be invalidated and allows to subscribe other invalidables
  abstract class Value[T](protected var value_ : T)
    extends (() => T) with HasInternalSubscribers with HasExternalSubscribers[T]
  {
    // iff dirty the current value (to be defined) needs to be recalculated
    protected var dirty = false

    // when the value was updated last time
    private var timestamp = -1

    protected def updateValue(x : T) = {
      if (value_ != x)
      {
        value_ = x
        invalidate()
        true
      }
      else false
    }

    // marks the node as dirty
    // and all dependent nodes too
    def invalidate() : Unit = {
      if (!dirty) {
        dirty = true
        internal foreach { _ invalidate () }
      }
    }

    // [protected] interface
    // if we have external listeners they should be notified when an update is done
    def notifyExternalListeners() : Unit =
      if (timestamp != globalUpdateId)
        {
          timestamp = globalUpdateId
          if (external.nonEmpty) {
            val oldValue = value_
            val newValue = apply()
            if (oldValue != newValue)
              external foreach { _ apply newValue }
              internal foreach { _ notifyExternalListeners ()}
          }
        }

    protected def validate()

    def apply() = {
      if (timestamp != globalUpdateId) {
        timestamp = globalUpdateId
        if (dirty) {
          validate()
          dirty = false
        }
      }
      value_
    }

  }

  case class Variable[T](initialValue : T) extends Value[T](initialValue)
  {
    protected def validate() {}
    val inputs = Nil

    finalConstruct()

    def set(x : T) = {
      if (updateValue(x))
        {
          external foreach { _ apply x }
          internal foreach { _ notifyExternalListeners () }
        }
    }
  }

  case class Unary[A,Result](a : Value[A])(f : A => Result)
    extends Value[Result](f(a()))
  {
    private var cachedA = a()

    val inputs = a :: Nil
    finalConstruct()

    def validate() = {
      if (cachedA != a()) {
        cachedA = a()
        updateValue(f(cachedA))
      }
    }
  }

case class Binary[A,B, Result](a : Value[A], b : Value[B])(f : (A,B) => Result)
  extends Value[Result](f(a(), b()))
{
  private var cachedA = a()
  private var cachedB = b()

  val inputs = a :: b :: Nil

  finalConstruct()

  def validate() = {
    if (cachedA != a() || cachedB != b()) {
      cachedA = a()
      cachedB = b()
      updateValue(f(cachedA, cachedB))
    }
  }
}

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

    def validate() = {
      if (cachedCondition != condition())
          cachedCondition = condition()

      if (cachedCondition && ifBranch() != cachedIf)
          cachedIf = ifBranch()

      if (!cachedCondition && elseBranch() != cachedElse)
          cachedElse = elseBranch()

      updateValue(if (cachedCondition) cachedIf else cachedElse)
    }
  }

  object Ops {
    implicit class ValueStringOps (self : Value[String]) {
      def Parenthise = Unary(self) { s => s"($s)" }
      def + (other : Value[String]) = Binary(self, other) { _ + " " + _ }
    }

    implicit class ValueBooleanOps(self : Value[Boolean]) {
      def Then(ifBranch : Value[String]) = new {
        def Else(elseBranch : Value[String]) = IfThenElse(self, ifBranch, elseBranch)
      }
    }
  }

  def main(args : Array[String]) =
  {
    import Ops._

    val condition = Variable(true)

    val a = Variable("a0")
    val b = Variable("b0")
    val c = Variable("c0")

    val ab = condition Then a Else b
    val d = c.Parenthise

    val e = Variable("e0")

    val abd = (ab + d + ab).Parenthise

    val abde = abd + e

    def trace(label : String)(x : String) = println(label + " -> " + x)
    a += trace("A")
    b += trace("B")
    c += trace("C")
    ab += trace("If condition Then A Else B")
    d += trace("C.Parenthize")
    abd += trace("AB + D + AB")

    def state = s"A = ${a()}, B = ${b()}, c = ${c()}, e = ${e()}, cond = ${condition()}: "

    def print(header : String, value : => Unit) = {
      println()
      println(s"------ $header ---------")
      val executed = value
      println(state + s" ABDE = ${abde()}")
    }

    print("", ())

    print("c set \"c1\"", c set "c1")

    print("condition set false", condition set false)

    print("a set \"a1\"", a set "a1")

    print("b set \"b1\"", b set "b1")

    print("e set \"e1\"", e set "e1")
  }
}