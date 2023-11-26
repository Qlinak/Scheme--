package ass3

trait InterpreterInterface:
  self: LispImpl =>
  def eval(x: Data)(using env: Environment[Data])(using tydefs: Environment[List[String]]): Data
  def evaluate(x: Data): Data
  def evaluate(s: String): String
