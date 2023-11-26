package ass3

trait Helpers:
  self: LispImpl =>
  def mkLambda(ps: List[String], body: Data)(using env: Environment[Data])(using tydefs: Environment[List[String]]) =
    Lambda { args => eval(body)(using env.extendMulti(ps, args)) }

  def asList(x: Data): List[Data] = x match
    case xs: List[_] => xs
    case _ => throw SyntaxError("malformed list: " + x)

  def toName(s: String, upper: Boolean)(x: Data): String = x match
    case Symbol(name) if name.head.isUpper == upper => name
    case _ => throw SyntaxError("malformed " + s + ": " + x)

  val paramName = toName("parameter", false)
  val fieldName = toName("field", false)
  val className = toName("class", true)
