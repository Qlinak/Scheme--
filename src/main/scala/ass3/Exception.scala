package ass3

trait Exception:
  self: LispImpl =>
  abstract class LispException(msg: String) extends java.lang.RuntimeException(msg)

  case class ParseError(msg: String) extends LispException(msg)

  case class SyntaxError(msg: String) extends LispException(msg)

  case class UndefinedSymbol(msg: String) extends LispException(msg)

  case class ClassArityMismatch(msg: String) extends LispException(msg)

  case class FunArityMismatch(msg: String) extends LispException(msg)

  case class MatchError(msg: String) extends LispException(msg)

  case class SelError(msg: String) extends LispException(msg)

  case class FieldError(msg: String) extends LispException(msg)

  case class AppError(msg: String) extends LispException(msg)
