package object ops
{

    trait ConversionDefined[From, To]
    {
        def convert(from : From) : To
    }

    trait PlusDefined[A,B]
    {
        type Ret
        def plus(a : A, b : B) : Ret
    }

    object Addable {
        implicit def fromNumeric[T : Numeric] : PlusDefined[T,T] = new PlusDefined[T,T]
        {
            type Ret = T
            def plus(a : T, b : T) = implicitly[Numeric[T]].plus(a,b)
        }
    }


}

