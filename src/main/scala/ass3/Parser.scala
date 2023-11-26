package ass3

trait Parser:
  self: LispImpl =>
  def string2lisp(s: String): Data =
    val it = new LispTokenizer(s)

    def parseExpr(token: String): Data =
      if token == "(" then parseList
      else if token == ")" then throw ParseError("unmatched parenthesis")
      else if token.charAt(0).isDigit then token.toInt
      else if token.charAt(0) == '\"' && token.charAt(token.length()-1) == '\"' then token.substring(1,token.length - 1)
      else Symbol(token)

    def parseList: List[Data] =
      val token = it.next
      if token == ")" then Nil else parseExpr(token) :: parseList

    parseExpr(it.next)

  def lisp2string(x: Data): String = x match
    case Symbol(name) =>
      name
    case xs: List[_] =>
      xs.map(lisp2string).mkString("(", " ", ")")
    case _ =>
      x.toString
