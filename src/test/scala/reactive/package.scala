import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec

package object reactive {

    trait TestBase extends FlatSpec with MockFactory
    {
        def variable(initialValue : String) = new {
            class Var extends Variable(initialValue){
                override def toString() = initialValue
            }
            val value = new Var()
            val handler = mockFunction[String, Unit]("event")
            value += handler
        }

        def toUpperCase(source : Value[String], initialValue : String) = new {
            val back = mockFunction[String, Unit]("back")
            back expects initialValue once ()

            def impl(s : String) = {
                back(s)
                s.toUpperCase
            }

            class ToUpperCase extends UnaryBase(source, impl(initialValue))
            {
                override def F(s : String) = impl(s)

                override def toString() = s"ToUpperCase($source)"
            }

            val value = new ToUpperCase

            val handler = mockFunction[String, Unit]
            value += handler
        }

        def concat(a : Value[String],
                   b : Value[String],
                   initialValueA : String,
                   initialValueB : String)
        = new {
            val back = mockFunction[String, String, Unit]("concat")
            back expects (initialValueA, initialValueB) once ()

            def impl(x : String, y : String) = {
                back(x,y)
                s"($x$y)".toUpperCase
            }

            case class Concat() extends BinaryBase(a,b,impl(initialValueA, initialValueB))
            {
                override def F(x : String, y : String) = impl(x, y)

                override def toString() = s"Concat($a, $b)"
            }

            val value = Concat()

            val handler = mockFunction[String, Unit]
            value += handler
        }
    }

}
