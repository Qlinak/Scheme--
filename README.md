# Lisp interpreter
A interpreter for scripting language Scheme-- (resembling Lisp) written in Scala. It has the following features
1. Basic [Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language))-like evaluation, including val binding, def binding, recursive calls and list manipulation
2. [Object construction](#object-construction)
3. [Object fields selection](#field-selection)
4. [Pattern matching](#pattern-matching)
5. [Call by need evaulation](#call-by-need)


## Object construction

An object of a class can be constructed by applying the constructor to a sequence of arguments.
For example, the following constructs an object of class `Pair` with the values `1` and `2` for fields `x` and `y` respectively:
```clojure
(class (Pair x y) (Pair 1 2))
```

The number of arguments applied to the constructor must match exactly the arity of the class declaration.
Otherwise, a `ClassArityMismatch` with the message `"wrong arity for class <name>"` is thrown, where `<name>` is the class name.
Just like functions, constructors cannot be partially applied.
For example, both of the following result in an error:
```clojure
(class (Pair x y) (Pair 1))
(class (Pair x y) (Pair 1 2 3))
```

The behavior of evaluating an unapplied constructor is unspecified. For example, the behavior of the following program is unspecified:
```clojure
(class (Pair x y) Pair)
```

## Field selection

Fields of an object can be accessed using the field selection special form:
```clojure
(sel <expr> <field>)
```

`<expr>` is first evaluated.
If `<expr>` evaluates to an object `o` of class `C`, and
(1) `C` has field `<field>`, then the whole expression evaluates to the value of the field `<field>` of `o`;
(2) `C` does not have field `<field>`, then a `FieldError` with message `"class C has no field <field>"` is thrown.
If `<expr>` evaluates to something other than an object, then a `SelError` with the message `"selection from a non-object: o"` is thrown.

## Pattern matching

The class of an object can be inspected at runtime using pattern matching, with the following special form:
```bnf
(case <scrut> { <branch> })
```
where case branches `<branch>` are one of the following.
```bnf
<branch> =   ((<name> { <param> }) <expr>)
           | (<param> <expr>)
```

`<scrut>` is first evaluated to `v`, then matched against `{ <branch> }` in order.
If a branch is matched, then that branch is taken.
The remaining branches would not be processed.
If a branch is not matched, then pattern matching proceeds to the next branch.
If all branches are not matched or `v` is not an object, then a MatchError with the message `"match error on: <scrut>"` is thrown.

If a branch is in the first form `((<name> { <param> }) <expr>)`,
then the class definition of class `<name>` is looked up.
The size of `{ <param> }` must match exactly the arity of class `<name>`,
otherwise a `ClassArityMismatch` with the message `"wrong arity for class <name>"` is thrown.
If `v` is an object of class `<name>`, then the branch is matched,
and `<expr>` is evaluated in an environment extended by binding each of `{ <param> }` to the respective fields of `v`.
For example, in the following expression, `<expr>` is evaluated in an environment extended with bindings `a -> 1` and `b -> 2`:
```clojure
(class (Pair x y) (case (Pair 1 2) ((Pair a b) <expr>)))
```

If a branch is in the second form `(<param> <expr>)`, then it is always matched.
`<expr>` is evaluated in an environment extended by binding `<param>` to `v`.
For example, in the following expression, `<expr>` is evaluated in an environment extended with binding `x -> Pair 1 2`:
```clojure
(class (Pair x y) (case (Pair 1 2) (x <expr>)))
```

If a branch does not match either form, then a `SyntaxError` with the message `"invalid case branch: <branch>"` is thrown.

## Call-by-need

Call-by-need is similar to call-by-name, where the arguments are not evaluated until their value is accessed.
However, unlike call-by-name, the arguments are only evaluated once,
after which the result is stored for further access.

For val bindings, they are accessed only when the bound variable is evaluated inside the body.
For example, in the following program, the expression `<expr>` is never evaluated:
```clojure
(val x <expr> 0)
```
In the following program, the expression `<expr>` is only evaluated once:
```clojure
(val x <expr> (* x x))
```

For function arguments, they are accessed only when the corresponding parameter is evaluated inside the function body.
For example, in the following program, the expressions `(id <expr>)` and `<expr>` are never evaluated:
```clojure
(def id (lambda (x) x) (def zero (lambda (x) 0) (zero (id <expr>))))
```
In the following program, the expressions `(sq (sq <expr>))`, `(sq <expr>)`, and `<expr>` are each only evaluated once:
```clojure
(def sq (lambda (x) (* x x)) (sq (sq (sq <expr>))))
```

For constructor arguments, they are accessed only when a selection is evaluated,
or when a variable bound by pattern matching is evaluated.
Namely, constructing an object does not evaluate any of the arguments.
For example, in the following programs, the expressions `<expr1>` and `<expr2>` are never evaluated:
```clojure
(class (Pair x y) (def zero (lambda (x) 0) (val x (Pair <expr1> <expr2>) (zero (sel x x)))))
(class (Pair x y) (def zero (lambda (x) 0) (case (Pair <expr1> <expr2>) ((Pair a b) (zero a)))))
```
In the following programs, the expression `<expr1>` is evaluated once, while `<expr2>` is never evaluated:
```clojure
(class (Pair x y) (val x (Pair <expr1> <expr2>) (* (sel x x) (sel x x))))
(class (Pair x y) (case (Pair <expr1> <expr2>) ((Pair a b) (* a a))))
```
