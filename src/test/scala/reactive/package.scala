import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec

package object reactive {

    trait TestBase extends FlatSpec with MockFactory
    {
        def variable[T](initialValue : T) = new {
            class Var extends Variable(initialValue, initialValue.toString)
            val value : Variable[T] = new Var()
            val handler = mockFunction[T, Unit]("event")
            value += handler
        }

        def ifThenElse[T](condition     : Signal[Boolean],
                          ifBranch      : Signal[T],
                          elseBranch    : Signal[T])
        = new {
            val value = IfThenElse(condition, ifBranch, elseBranch)
            val handler = mockFunction[T, Unit]("event")
            value += handler
        }

        class ToUpperCaseBase(source : Signal[String], initialValue : String)
        {
            val back = mockFunction[String, Unit]("back")
            back expects initialValue once ()

            def impl(s : String) = {
                back(s)
                s.toUpperCase
            }

            class ToUpperCase extends UnaryBase(source, impl(initialValue), "toUpper")
            {
                override def F(s : String) = impl(s)

                override def toString() = s"ToUpperCase($source)"
            }
            
            val value = new ToUpperCase
        }

        def toUpperCase(source : Signal[String], initialValue : String) =
            new ToUpperCaseBase(source, initialValue)

        def toUpperCaseHandled(source : Signal[String], initialValue : String) =
            new ToUpperCaseBase(source, initialValue) 
            {
                val handler = mockFunction[String, Unit]
                value += handler
            }

        def concat(a : Signal[String],
                   b : Signal[String],
                   initialValueA : String,
                   initialValueB : String)
        = new {
            val back = mockFunction[String, String, Unit]("concat")
            back expects (initialValueA, initialValueB) once ()

            def impl(x : String, y : String) = {
                back(x,y)
                s"($x$y)".toUpperCase
            }

            case class Concat() extends BinaryBase(a,b,impl(initialValueA, initialValueB),"+")
            {
                override def F(x : String, y : String) = impl(x, y)
            }

            val value = Concat()

            val handler = mockFunction[String, Unit]("concatHandler")
            value += handler
        }
    }

}
