package ass3

trait Interpreter:
  self: LispImpl =>
  import language.deprecated.symbolLiterals
  class Lazy(expr: => Data)(using env: Environment[Data])(using tydefs: Environment[List[String]]) {
    private lazy val evaluatedExpr: Data = eval(expr)
    def force(): Data =
      evaluatedExpr

    override def toString: String = s"Lazy(${this.force()})"
  }

  def eval(x: Data)(using env: Environment[Data])(using tydefs: Environment[List[String]]): Data = x match
    case _: String =>
      x
    case _: Int =>
      x
    case Symbol(name) =>
      if name.head.isUpper
      then tydefs.lookup(name)
      else forceLazy(env.lookup(name))
    case Symbol("val") :: param :: expr :: rest :: Nil =>
      eval(rest)(using env.extend(paramName(param), applyLazy(expr)))
    case Symbol("def") :: param :: expr :: rest :: Nil =>
      eval(rest)(using env.extendRec(paramName(param), env1 => eval(expr)(using env1)))
    case Symbol("if") :: cond :: thenpart :: elsepart :: Nil =>
      if eval(cond) != 0 then eval(thenpart)
      else eval(elsepart)
    case Symbol("quote") :: y :: Nil =>
      y
    case Symbol("lambda") :: params :: body :: Nil =>
      mkLambda(asList(params).map(paramName), body)
    case Symbol("class") :: nameNFields :: rest :: Nil => nameNFields match
      case name :: fields =>
        eval(rest)(using env)(using tydefs.extend(className(name), fields.map(fieldName)))
    case Symbol("sel") :: expr :: field :: Nil =>
      val res = eval(expr)
      res match
        case _: Map[String, Any] =>
          val map = res.asInstanceOf[Map[String, Any]]
          field match
            case Symbol(name) =>
              try {
                forceLazy(map(name))
              } catch {
                case _: NoSuchElementException => throw FieldError(s"class ${map("class")} has no field $name")
              }
            case _ => throw AppError(s"field $field is not a Symbol")
        case x => throw SelError(s"selection from a non-object: $x")
    case Symbol("case") :: scrut :: branches =>
      val res = eval(scrut)
      val field2ValMap = res.asInstanceOf[Map[String, Any]]
      if field2ValMap == null
      then throw MatchError(s"match error on: $scrut")
      else branches match
        case Nil => throw MatchError(s"match error on: $scrut")
        case curBranch :: rest => curBranch match
          case nameNParams :: List(expr) => nameNParams match
            case name :: params => // has params
              if name.asInstanceOf[Symbol].name.head.isLower
              then throw SyntaxError(s"invalid case branch: $curBranch")
              else if name.asInstanceOf[Symbol].name == field2ValMap("class")
              then
                val fields = tydefs.lookup(field2ValMap("class").toString)
                if params.length == fields.length
                then
                  val arg2FieldMap = params.map(p => p.asInstanceOf[Symbol].name).zip(fields).toMap
                  val arg2ValMap = arg2FieldMap.map { case (key, value) => key -> forceLazy(field2ValMap(value)) }
                  eval(expr)(using env.extendMulti(arg2ValMap.keys.toList, arg2ValMap.values.toList))
                else throw ClassArityMismatch(s"wrong arity for class ${field2ValMap("class")}")
              else eval(Symbol("case") :: scrut :: rest)
            case Symbol(name) => // no params
              if name.head.isLower
              then eval(expr)(using env.extend(name, field2ValMap))
              else throw SyntaxError(s"invalid case branch: $curBranch")
            case _ => throw SyntaxError(s"invalid case branch: $curBranch")
    case operator :: operands => eval(operator) match
      case Lambda(f) =>
        f(operands.map(applyLazy))
      case fields @ _ :: _ => // fields of a type class
        if fields.length == operands.length
        then
          val res: Map[Data, Data] = fields.zip(operands.map(applyLazy)).toMap // Map(fieldName -> value)
          res + ("class"-> operator.asInstanceOf[Symbol].name) // also store class name
        else throw ClassArityMismatch(s"wrong arity for class ${operator.asInstanceOf[Symbol].name}")
      case x =>
        throw AppError("application of a non-function: " + x + " to " + operands)

  def evaluate(x: Data): Data = eval(x)(using globalEnv)(using emptyEnvironment)

  def evaluate(s: String): String = lisp2string(evaluate(string2lisp(s)))
