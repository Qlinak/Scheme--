package ass3

trait LispImpl extends Interpreter with Tokenizer with Parser with Envir with Helpers with Exception

object Lisp extends LispImpl with InterpreterInterface
