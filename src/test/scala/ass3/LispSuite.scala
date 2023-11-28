package ass3

/**
 * This class is a test suite for the methods in object Lisp.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class LispSuite extends munit.FunSuite:
  import language.deprecated.symbolLiterals
  import Lisp.evaluate
  import Lisp.FieldError
  import Lisp.ClassArityMismatch
  import Lisp.SelError
  import Lisp.string2lisp
  import Lisp.SyntaxError
  import Lisp.MatchError
  import Lisp.Lazy

  test("Field selection 1") {
    assertEquals(evaluate("(class (Pair x y) (val p (Pair \"zero\" 0) (sel p x))))))"), "zero")
    assertEquals(evaluate("(class (Pair x y) (val p (Pair \"zero\" 0) (sel p y))))))"), "0")
  }

  test("Field selection 2") {
    assertEquals(evaluate("(class (Pair x y) (class (Riap y x) (def f (lambda (p) (sel p x)) (val p (Pair 2 3) (val r (Riap 2 3) (* (f p) (f r)))))))"), "6")
  }

  test("Field selection 3") {
    assertEquals(evaluate("(class (Pair x y) (val a 3 (val b 4 (sel (Pair a b) x))))"), "3")
  }

  test("ClassArityMismatch") {
    val ex = intercept[ClassArityMismatch] {
      assertEquals(evaluate("(class (Pair x y) (val p (Pair \"zero\" 0 0) (sel p x))))))"), "zero")
      assertEquals(evaluate("(class (Pair x y) (val p (Pair \"zero\") (sel p y))))))"), "zero")
    }

    intercept[ClassArityMismatch] {
      assertEquals(evaluate("(class (Pair x y) (val p (Pair \"zero\") (sel p y))))))"), "zero")
    }

    assertEquals(ex.msg, "wrong arity for class Pair")

    intercept[ClassArityMismatch] {
      assertEquals(evaluate("(class (Pair x y) (class (Triple x y z) (def prod (lambda (x) (case x ((Pair x y) (* x y)) ((Triple x y) (* (* x y) z)))) (val x (Triple 2 3 7) (prod x)))))"), "42")
    }
  }

  test("FieldError") {
    val ex = intercept[FieldError] {
      assertEquals(evaluate("(class (Pair x y) (val p (Pair \"zero\" 0) (sel p z))))))"), "zero")
    }

    assertEquals(ex.msg, "class Pair has no field z")
  }

  test("SelError") {
    val ex1 = intercept[SelError] {
      assertEquals(evaluate("(class (Pair x y) (val p (Pair \"zero\" 0) (sel 3 z))))))"), "zero")
    }

    val ex2 = intercept[SelError] {
      assertEquals(evaluate("(class (Pair x y) (class (Riap y x) (def f (lambda (p) (sel p x)) (val p (Pair 2 3) (val r (Riap 2 3) (sel f x))))))"), "6")
    }

    assertEquals(ex1.msg, "selection from a non-object: 3")
    assertEquals(ex2.msg, "selection from a non-object: Lambda(<function1>)")
  }

  test("SyntaxError"){
    intercept[SyntaxError] {
      assertEquals(evaluate("(class (Pair x y) (class (Triple x y z) (def f (lambda (x) (case x ((Pair x y) (* x y)) (X (cons x nil)))) (val x (Triple 2 3 7) (sel (car (f x)) z)))))"), "7")
    }

    intercept[SyntaxError] {
      assertEquals(evaluate("(class (Pair x y) (class (Triple x y z) (def prod (lambda (x) (case x (324 (* x y)) ((Triple x y z) (* (* x y) z)))) (val x (Triple 2 3 7) (prod x)))))"), "42")
    }

    intercept[SyntaxError] {
      assertEquals(evaluate("(class (Pair x y) (class (Triple x y z) (def prod (lambda (x) (case x ((Pair x y) (* x y)) ((triple x y z) (* (* x y) z)))) (val x (Triple 2 3 7) (prod x)))))"), "42")
    }
  }

  test("MatchError") {
    intercept[MatchError] {
      assertEquals(evaluate("(class (Pair x y) (class (Triple x y z) (def f (lambda (x) (case x ((Pair x y) (* x y)))) (val x (Triple 2 3 7) (sel (car (f x)) z)))))"), "7")
    }
  }

  test("Pattern matching 1") {
    assertEquals(evaluate("(class (Pair x y) (class (Triple x y z) (def prod (lambda (x) (case x ((Pair x y) (* x y)) ((Triple x y z) (* (* x y) z)))) (val x (Triple 2 3 7) (prod x)))))"), "42")
  }

  test("Pattern matching 2") {
    assertEquals(evaluate("(class (Pair x y) (class (Triple x y z) (def f (lambda (x) (case x ((Pair x y) (* x y)) (x (cons x nil)))) (val x (Triple 2 3 7) (sel (car (f x)) z)))))"), "7")
  }

  test("Pattern matching 3") {
    assertEquals(evaluate("(class (Pair x y) (class (Triple x y z) (def f (lambda (x) (case x (x (cons x nil)) ((Pair x y) (* x y)) )) (val x (Pair 2 3) (sel (car (f x)) y)))))"), "3")
  }

  test("Pattern matching 4") {
    assertEquals(evaluate("(class (Pair x y) (class (Triple x y z) (def f (lambda (x) (case x ((Pair a b) (Pair b a)) ((Triple a b c) (* (* a b) c)))) (val x (Pair 2 3) (sel (f x) y)))))"), "2")
  }

  test("Pattern matching 5") {
    assertEquals(evaluate("(class (Pair x y) (class (Triple x y z) (def f (lambda (x) (case x ((Pair a b) (* a a)) ((Triple a b c) (* (* a b) c)))) (val x (Pair 2 3) (f x)))))"), "4")
  }

  test("Pattern matching 6") {
    assertEquals(evaluate("(class (Pair x y) (case (Pair 3 4) ((Pair a b) (* a b))))"), "12")
  }

  test("Pattern matching 7") {
    assertEquals(evaluate("(class (Pair x y) (case (Pair 1 2) ((Triple a b c) (* c c)) ((Pair a b) (* a b))))"), "2")
  }

  test("call by need val 01") {
    assertEquals(evaluate("(val x (* 3 5) (val y (- 2 1) y))"), "1")
  }

  test("call by need val 02") {
    assertEquals(evaluate("(val x (* 2 2) (* x x))"), "16")
  }

  test("call by need val 03") {
    assertEquals(evaluate("(val x (* 2 2) (* (* x x) x))"), "64")
  }

  test("Call-by-need: def 1") {
    assertEquals(evaluate("(def zero (lambda (x) 0) (zero nani))"), "0")
  }

  test("Call-by-need: def 2") {
    assertEquals(evaluate("(class (Just x) (val x (Just nani) 0))"), "0")
  }

  test("Call-by-need: def 3") {
    assertEquals(evaluate("(def id (lambda (x) x) (def zero (lambda (x) 0) (zero (id (* 2 2)))))"), "0")
  }

  test("Call-by-need: def 4") {
    assertEquals(evaluate("(def sq (lambda (x) (* x x)) (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq 1)))))))))))))))))))))))))))))))))))))))))))))))))))))))))"), "1")
  }

  test("factorial") {
    assertEquals(evaluate("(def factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1))))) (factorial 3))"), "6")
  }

  test("lambda") {
    assertEquals(evaluate("(def f (lambda (x) (* x x)) (f 3))"), "9")
  }

  test("val expr") {
    assertEquals(evaluate("(val x 2 (val y 3 (val z 7 (* (* x y) z))))"), "42")
  }

  test("ge val ="){
    assertEquals(evaluate("(val x (= 3 3) (val y (= 2 2) (= x x)))"), "1")
  }

  test("ge null?") {
    assertEquals(evaluate("(val x (* 3 3) (null? x))"), "0")
    assertEquals(evaluate("(val x (* 3 3) (null? nil))"), "1")
  }

  test("ge list manipulation 1") {
    assertEquals(evaluate("(def f (lambda (x) (cons x nil)) (car (f 5)))"), "5")
    assertEquals(evaluate("(car (cons 9 (cons 4 (cons 3(cons 2 nil)))))"), "9")
  }

  test("ge list manipulation 2") {
    assertEquals(evaluate("(car (cdr (cons 5 (cons 4 (cons 3 nil)))))"), "4")
  }

  test("ge list manipulation 3") {
    assertEquals(evaluate("(car (cdr (cdr (cons 5 (cons 4 (cons 3 nil))))))"), "3")
  }

  test("ge list manipulation 4") {
    intercept[scala.MatchError] {
      assertEquals(evaluate("(car (cdr (cdr (cdr (cons 5 (cons 4 (cons 3 nil)))))))"), "3")
    }
  }

  test("ge +") {
    assertEquals(evaluate("(+ \"ne\" \"ville\")"), "neville")
    assertEquals(evaluate("(+ 3 4)"), "7")
  }


