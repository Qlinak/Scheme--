package ass3

/**
 * This class is a test suite for the methods in object Lisp.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class LispSuite extends munit.FunSuite:
  import language.deprecated.symbolLiterals
  import Lisp.evaluate

  test("Field selection 1") {
    assertEquals(evaluate("(class (Pair x y) (val p (Pair \"zero\" 0) (sel p x))))))"), "zero")
    assertEquals(evaluate("(class (Pair x y) (val p (Pair \"zero\" 0) (sel p y))))))"), "0")
  }

  test("Field selection 2") {
    assertEquals(evaluate("(class (Pair x y) (class (Riap y x) (def f (lambda (p) (sel p x)) (val p (Pair 2 3) (val r (Riap 2 3) (* (f p) (f r)))))))"), "6")
  }

  test("Pattern matching 1") {
    assertEquals(evaluate("(class (Pair x y) (class (Triple x y z) (def prod (lambda (x) (case x ((Pair x y) (* x y)) ((Triple x y z) (* (* x y) z)))) (val x (Triple 2 3 7) (prod x)))))"), "42")
  }

  test("Pattern matching 2") {
    assertEquals(evaluate("(class (Pair x y) (class (Triple x y z) (def f (lambda (x) (case x ((Pair x y) (* x y)) (x (cons x nil)))) (val x (Triple 2 3 7) (sel (car (f x)) z)))))"), "7")
  }

  test("Call-by-need: no need 1") {
    assertEquals(evaluate("(def zero (lambda (x) 0) (zero nani))"), "0")
  }

  test("Call-by-need: no need 2") {
    assertEquals(evaluate("(class (Just x) (val x (Just nani) 0))"), "0")
  }
