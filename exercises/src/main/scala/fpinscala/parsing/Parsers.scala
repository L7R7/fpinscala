package fpinscala.parsing

import java.util.regex.Pattern

import fpinscala.testing.{Gen, Prop}

import scala.language.higherKinds
import scala.util.matching.Regex

trait Parsers[Parser[+ _]] {
  self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // run(char(c))(c.toString) == Right(c)
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  //  def string(s: String): Parser[String] // run(string(s))(s) == Right(s)
  def orString(s1: String, s2: String): Parser[String]

  // run(or(string("abra"), string("cadabra")))("abra") == Right("abra")
  // run(or(string("abra"), string("cadabra")))("cadabra") == Right("cadabra")
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  // run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
  // run(listofN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
  // run(listofN(3, "ab" | "cad"))("ababab") == Right("ababab")
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(a => succeed(f(a)))

  // run(succeed(a))(s) == Right(a)
  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  // run(slice(('a' | 'b').many))("aaba") == Right("aaba")
  def slice[A](p: Parser[A]): Parser[String]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p)(a => p2.map(b => (a, b)))

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p)(a => p2.map(b => f(a, b))) // could also be solved using a for-comprehension

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def root[A](p: Parser[A]): Parser[A] =
    p <* eof

  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_, b) => b)

  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a, b) => a)

  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  def whitespace: Parser[String] = "\\s*".r

  def attempt[A](p: Parser[A]): Parser[A]

  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    start *> p <* stop

  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = // use `Parser[Any]` since don't care about result type of separator
    sep1(p, p2) or succeed(List())

  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  def opL[A](p: Parser[A])(op: Parser[(A, A) => A]): Parser[A] =
    map2(p, many(op ** p))((h, t) => t.foldLeft(h)((a, b) => b._1(a, b._2)))

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def thru(s: String): Parser[String] = (".*?" + Pattern.quote(s)).r

  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  /** Unescaped or escaped string literals, like "An \n important \"Quotation\"" or "bar". */
  def escapedQuoted: Parser[String] =
  // rather annoying to write, left as an exercise
  // we'll just use quoted (unescaped literals) for now
    token(quoted label "string literal")

  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  def double: Parser[Double] =
    doubleString map (_.toDouble) label "double literal"

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def many1: Parser[List[A]] = self.many1(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def slice: Parser[String] = self.slice(p)

    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def *>[B](p2: => Parser[B]): Parser[B] = self.skipL(p, p2)

    def <*(p2: => Parser[Any]): Parser[A] = self.skipR(p, p2)

    def token: Parser[A] = self.token(p)

    def sep(separator: Parser[Any]): Parser[List[A]] = self.sep(p, separator)

    def sep1(separator: Parser[Any]): Parser[List[A]] = self.sep1(p, separator)

    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)

    def opL(op: Parser[(A, A) => A]): Parser[A] = self.opL(p)(op)

    def label(msg: String): Parser[A] = self.label(msg)(p)

    def scope(msg: String): Parser[A] = self.scope(msg)(p)
  }

  val numA: Parser[Int] = char('a').many.map(_.size)
  val numA2: Parser[Int] = char('a').many.slice.map(_.length) // size was replaced by length
  // run(numA)("aaa") == Right(3)
  // run(numA)("b") == Right(0)
  val zeroOrMoreAOneOrMoreB: Parser[(Int, Int)] = char('a').many.slice.map(_.length) ** char('b').many1.slice.map(_.length)
  val numberAndThatManyAs = for {
    digit <- "[0-9]+".r
    n = digit.toInt // we really should catch exceptions thrown by toInt and convert to parse failure
    _ <- listOfN(n, char('a'))
  } yield n

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p.map(a => a), p)(in)


    def productLaw[A, B, C](a: Parser[A], b: Parser[B], c: Parser[C])(in: Gen[String]): Prop = {
      def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)

      def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)

      equal((a ** b) ** c map unbiasL, a ** (b ** c) map unbiasR)(in)
    }

    //    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
    //      Prop.forAll(inputs ** Gen.string) { case (input, msg) =>
    //      run(label(msg)(p))(input match {
    //        case Left(e) => Location.errorMessage(e) == msg
    //        case _ => true
    //      })}
  }
}

case class Location(input: String, offset: Int = 0) {
  lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  //  lazy val col: Int = input.slice(0, offset + 1).reverse.indexOf('\n')
  //
  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  //  def advanceBy(n: Int) = copy(offset = offset + n)
  //
  //  /* Returns the line corresponding to this location */
  //  def currentLine: String =
  //    if (input.length > 1) input.lines.drop(line - 1).next
  //    else ""
}

object Location {
  def errorLocation(e: ParseError): Location = ???

  def errorMessage(e: ParseError): String = ???
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {

  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)
}

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Err, Parser[+ _]](P: Parsers[Parser]): Parser[JSON] = {
    import P.{string => _, _}
    implicit def tok(s: String): Parser[String] = token(P.string(s))

    def array = surround("[", "]")(
      value sep "," map (vs => JArray(vs.toIndexedSeq))) scope "array"

    def obj = surround("{", "}")(
      keyval sep "," map (kvs => JObject(kvs.toMap))) scope "object"

    def keyval = escapedQuoted ** (":" *> value)

    def lit = scope("literal") {
      "null".as(JNull) |
        double.map(JNumber) |
        escapedQuoted.map(JString) |
        "true".as(JBool(true)) |
        "false".as(JBool(false))
    }

    def value: Parser[JSON] = lit | obj | array

    root(whitespace *> (obj | array))
  }
}

//object Naive {
//  type Parser[+A] = Location => Result[A]
//
//  sealed trait Result[+A] {
//    def mapError(f: ParseError => ParseError): Result[A] = this match {
//      case Failure(e) => Failure(f(e))
//      case _ => this
//    }
//  }
//  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
//  case class Failure(get: ParseError) extends Result[Nothing]
//
//  object Dancecompany extends Parsers[Parser] {
//    def string(s: String): Parser[String] =
//      (location: Location) =>
//        if (location.input.substring(location.offset).startsWith(s)) Success(s, location.offset + s.length)
//        else Failure(???)
//
//    //    def regex(r: Regex): Parser[String] =
//    //      location =>
//    def scope[A](msg: String)(p: Parser[A]): Parser[A] =
//      s => p(s).mapError(_.push(s.loc, msg))
//
//  }
//
//}