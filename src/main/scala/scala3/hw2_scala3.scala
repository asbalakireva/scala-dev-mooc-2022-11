package scala

// 1
object Operators:
  extension (x: Int)
     def ++(y: Int): Int = (x.toString + y.toString).toInt

  @main def OperatorsEx() =
      println(2 ++ 35) // просто с + будет складывать как обычно,  не поняла как сделать, чтобы  с одним + работал

end Operators

// 2
object Completions:

  // The argument "magnet" type
  enum CompletionArg:
    case ShowItIsString(s: String)
    case ShowItIsInt(i: Int)
    case ShowItIsFloat(f: Float)

  object CompletionArg:
    given fromString: Conversion[String, CompletionArg] = ShowItIsString(_)
    given fromInt: Conversion[Int, CompletionArg] = ShowItIsInt(_)
    given fromFloat: Conversion[Float, CompletionArg] = ShowItIsFloat(_)
  end CompletionArg

  import CompletionArg.*

  def complete[T](arg: CompletionArg) = arg match
    case ShowItIsString(s) => println(s"${s} - String")
    case ShowItIsInt(i) => println(s"${i} - Int")
    case ShowItIsFloat(f) => println(s"${f} - Float")

  @main def CompletionsEx() = complete("abc")
end Completions


// 3

object MyMath:

  opaque type Logarithm = Double
  object Logarithm:

    def apply(d: Double): Logarithm = math.log(d)

    def safe(d: Double): Option[Logarithm] =
      if d > 0.0 then Some(math.log(d)) else None

  end Logarithm

  extension (x: Logarithm)
    def toDouble: Double = math.exp(x)
    def + (y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
    def * (y: Logarithm): Logarithm = x + y

  @main def MyMathEx() =
    val l = Logarithm(1.0)
    val l2 = Logarithm(2.0)
    val l3 = l * l2
    val l4 = l + l2
    println(l3)
    println(l4)

end MyMath
