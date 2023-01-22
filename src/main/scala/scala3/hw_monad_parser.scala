package scala

object MonadPackage {

  class ParserWithGivenParam(using val splitter: (String,String)):
    def mySplitter = summon[(String,String)]
    class Parser[T, Src](private val p: Src => (T, Src)):
      def flatMap[M](f: T => Parser[M, Src]): Parser[M, Src] =
        Parser { src =>
          val (word, rest) = p(src)
          f(word).p(rest)
        }

      def map[M](f: T => M): Parser[M, Src] =
        Parser { src =>
          val (word, rest) = p(src)
          (f(word), rest)
        }

      def parse(src: Src): T = p(src)._1


    object Parser:
      def apply[T, Src](f: Src => (T, Src)) = new Parser[T, Src](f)


    case class TestClass(field1: Int, field2: String, field3: Boolean)

    def stringField: Parser[String, String] = Parser[String, String] {
      str =>
      val idx = str.indexOf(mySplitter._2)
      if idx > -1 then
        (str.substring(0, idx), str.substring(idx + 1))
      else
        (str, "")
    }

    def intField: Parser[Int, String] = stringField.map(_.toInt)

    def booleanField: Parser[Boolean, String] = stringField.map(_.toBoolean)

    val parser =
      for
        field1 <- intField
        field2 <- stringField
        field3 <- booleanField
      yield TestClass(field1, field2, field3)

    def parsCsv(str: String) = str.split(mySplitter._1).map(parser.parse)


  @main def csvParser(): Unit = {

    val str = "1,test1,true\n2,test2,false,\n3,test3,true"
    val split = ParserWithGivenParam(using ("\n",","))
    val result = split.parsCsv(str)
    result

  }


}
