package ass3

trait Interpreter:
  self: LispImpl =>
  import language.deprecated.symbolLiterals

  def eval(x: Data)(using env: Environment[Data])(using tydefs: Environment[List[String]]): Data = x match
    case _: String => x
    case _: Int => x
    case Symbol(name) => env.lookup(name)
    case 'val :: param :: expr :: rest :: Nil =>
      eval(rest)(using env.extend(paramName(param), eval(expr)))
    case 'def :: param :: expr :: rest :: Nil =>
      eval(rest)(using env.extendRec(paramName(param), env1 => eval(expr)(using env1)))
    case 'if :: cond :: thenpart :: elsepart :: Nil =>
      if eval(cond) != 0 then eval(thenpart)
      else eval(elsepart)
    case 'quote :: y :: Nil => y
    case 'lambda :: params :: body :: Nil =>
      mkLambda(asList(params).map(paramName), body)
    case operator :: operands => eval(operator) match
      case Lambda(f) => f(operands.map(eval))
      case x => throw AppError("application of a non-function: " + x + " to " + operands)

  def evaluate(x: Data): Data = eval(x)(using globalEnv)(using emptyEnvironment)

  def evaluate(s: String): String = lisp2string(evaluate(string2lisp(s)))
