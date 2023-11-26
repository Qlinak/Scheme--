package ass3

trait Envir:
  self: LispImpl =>
  abstract class Environment[T]:
    def lookup(n: String): T

    def extendRec(name: String, expr: Environment[T] => T): Environment[T] =
      new Environment[T]:
        def lookup(n: String): T =
          if n == name then expr(this) else Environment.this.lookup(n)

    def extend(name: String, v: T): Environment[T] = extendRec(name, _ => v)

    def extendMulti(ps: List[String], vs: List[T]): Environment[T] = (ps, vs) match
      case (List(), List()) => this
      case (p :: ps1, arg :: args1) => extend(p, arg).extendMulti(ps1, args1)
      case _ => throw FunArityMismatch("wrong number of arguments")

  def emptyEnvironment[T] =
    new Environment[T]:
      def lookup(n: String): T = throw UndefinedSymbol("undefined: " + n)

  val globalEnv =
    emptyEnvironment[Data]
    .extend("=", Lambda {
      case List(arg1, arg2) => if arg1 == arg2 then 1 else 0})
    .extend("+", Lambda {
      case List(arg1: Int, arg2: Int) => arg1 + arg2
      case List(arg1: String, arg2: String) => arg1 + arg2})
    .extend("-", Lambda {
      case List(arg1: Int, arg2: Int) => arg1 - arg2})
    .extend("*", Lambda {
      case List(arg1: Int, arg2: Int) => arg1 * arg2})
    .extend("/", Lambda {
      case List(arg1: Int, arg2: Int) => arg1 / arg2})
    .extend("nil", Nil)
    .extend("cons", Lambda {
      case List(arg1, arg2) => arg1 :: asList(arg2)})
    .extend("car", Lambda {
      case List(x :: xs) => x})
    .extend("cdr", Lambda {
      case List(x :: xs) => xs})
    .extend("null?", Lambda {
      case List(Nil) => 1
      case _ => 0})
