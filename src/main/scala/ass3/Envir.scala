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
      case List(arg1: Lazy, arg2: Lazy) => if arg1.force() == arg2.force() then 1 else 0})
    .extend("+", Lambda {
      case List(arg1: Lazy, arg2: Lazy) =>
        val r1 = arg1.force()
        val r2 = arg2.force()
        if r1.isInstanceOf[Int] && r2.isInstanceOf[Int]
        then r1.asInstanceOf[Int] + r2.asInstanceOf[Int]
        else if r1.isInstanceOf[String] && r2.isInstanceOf[String]
        then r1.asInstanceOf[String] + r2.asInstanceOf[String]
        else throw MatchError(s"not support + for $r1 and $r2")
    })
    .extend("-", Lambda {
      case List(arg1: Lazy, arg2: Lazy) => arg1.force().asInstanceOf[Int] - arg2.force().asInstanceOf[Int]})
    .extend("*", Lambda {
      case List(arg1: Lazy, arg2: Lazy) => arg1.force().asInstanceOf[Int] * arg2.force().asInstanceOf[Int]})
    .extend("/", Lambda {
      case List(arg1: Lazy, arg2: Lazy) => arg1.force().asInstanceOf[Int] / arg2.force().asInstanceOf[Int]})
    .extend("nil", Nil)
    .extend("cons", Lambda {
      case List(arg1, arg2) => arg1 :: asList(arg2)})
    .extend("car", Lambda {
      case List(x: Lazy) =>
        val resList = x.force()
        try{
          asList(resList).head.asInstanceOf[Lazy].force()
        } catch {
          case _: SyntaxError => throw scala.MatchError("asList fails in car")
          case _: ClassCastException => throw scala.MatchError("cannot cast head into Lazy in car")
        }
      case List(x :: xs) => x.asInstanceOf[Lazy].force()
    })
    .extend("cdr", Lambda {
      case List(x :: xs) => xs})
    .extend("null?", Lambda {
      case List(Nil) => 1
      case List(x: Lazy) => if x.force() == Nil then 1 else 0
      case _ => 0})
