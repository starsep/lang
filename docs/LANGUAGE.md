---
---
# Language Description
starsep-lang is imperative language with functional features.
Its grammar is based on [Latte](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2016/Latte/) with my extensions.

# Program Structure
Program is list of functions.
The `main` function is executed.

# Functions
- can be recursive
- arguments are passed by value
- can be passed as arguments

# Types
- int
- char
- string
- bool
- float
- function type:
``` cpp
  void foo1() {}
  void foo2(int x) {}
  int foo3() { return 42; }
  int foo4(int x, int y) { return x + y; }
  void main() {
    Fn<void> bar1 = foo1;
    Fn<int -> void> bar2 = foo2;
    Fn<int> bar3 = foo3;
    Fn<int, int -> int> bar4 = foo4;
  }
```

# Variables
- declared by `Type VarName`
- can be initiated at declaration by `Type VarName = value`
- consts are declared by `let ConstName = value`
- if variable is not initiated then it gets default value: 0 for int, false for bool,
empty string for string, 0.0 for float, null char for char

# Operators
- arithmetic: + - * / %
- parenthesis ( )
- logical: ! && ||
- comparison: < > <= >= == !=
- assignment: = += -= *= /= %=
- ternary operator (like in C) `boolExpr ? expr1 : expr2`

# Comments
- single line comments start with `#` or `//`
- multiline comments C-style, start with `/*`, end with `*/`

# Control flow
- if
``` python
 if BoolExpr1 {
    // instructions1
  } elif BoolExpr2 {
    // instructions2
  } else {
    // instructions3
  }
```
- while
``` rust
 while BoolExpr {
    // instructions
  }
```
- loop is equivalent of `while true`
- for c++-like
``` rust
 for Oper1; BoolExpr; Oper2 {
    // instructions
  }
```
- foreach
``` rust
  for varName in iterable {
    // instructions
  }
```
