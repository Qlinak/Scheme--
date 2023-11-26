package ass3

trait Tokenizer:
  self: LispImpl =>
  class LispTokenizer(s: String) extends Iterator[String]:
    private var i = 0
    private def isDelimiter(ch: Char) = ch <= ' ' || ch == '(' || ch == ')'

    def hasNext: Boolean =
      while i < s.length() && s.charAt(i) <= ' ' do i = i + 1
      i < s.length()

    def next: String =
      if hasNext then
        val start = i
        var ch = s.charAt(i); i = i + 1
        if ch == '(' then "("
        else if ch == ')' then ")"
        else
          while i < s.length() && !isDelimiter(s.charAt(i)) do i = i + 1
          s.substring(start, i)
      else throw ParseError("more input expected")
