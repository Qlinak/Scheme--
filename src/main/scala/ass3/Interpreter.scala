package ass3

trait Interpreter:
  self: LispImpl =>
  import language.deprecated.symbolLiterals

  def eval(x: Data)(using env: Environment[Data])(using tydefs: Environment[List[String]]): Data = x match
    case _: String => 
      x
    case _: Int => 
      x
    case Symbol(name) =>
      if name.head.isUpper
      then tydefs.lookup(name)
      else env.lookup(name)
    case Symbol("val") :: param :: expr :: rest :: Nil =>
      eval(rest)(using env.extend(paramName(param), eval(expr)))
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
        eval(rest)(using env)(using tydefs.extendRec(className(name), _ => fields.map(fieldName)))
    case Symbol("sel") :: expr :: field :: Nil =>
      val res = eval(expr)
      res match
        case str : String => throw SelError(s"selection from a non-object: $str")
        case myInt : Int => throw SelError(s"selection from a non-object: $myInt")
        case Lambda(f) => throw SelError(s"selection from a non-object: ${Lambda(f)}")
        case _ =>
          val map = res.asInstanceOf[Map[String, Any]]
          field match
            case Symbol(name) =>
              try {
                map(name)
              } catch {
                case _: NoSuchElementException => throw FieldError(s"class ${map("class")} has no field $name")
              }
            case _ => throw AppError(s"field $field is not a Symbol")
    case operator :: operands => eval(operator) match
      case Lambda(f) => 
        f(operands.map(eval))
      case fields @ _ :: _ => // fields of a type class
        if fields.length == operands.length
        then
          val res: Map[Any, Any] = fields.zip(operands).toMap // Map(fieldName -> value)
          res + ("class"-> operator.asInstanceOf[Symbol].name) // also store class name
        else throw ClassArityMismatch(s"wrong arity for class ${operator.asInstanceOf[Symbol].name}")
      case x => 
        throw AppError("application of a non-function: " + x + " to " + operands)

  def evaluate(x: Data): Data = eval(x)(using globalEnv)(using emptyEnvironment)

  def evaluate(s: String): String = lisp2string(evaluate(string2lisp(s)))
