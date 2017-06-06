---
---

### starsep-lang has following features required in [task description](https://starsep.com/lang/task.pdf):

#### For 8 points:

- int values - **good/Int.stl**
- variables, assignments - **good/Assign.stl**
- if - **good/If.stl**
- while - **good/While.stl**
- expressions with + - * / ( ) - **good/Arithmetic.stl**
- comparison - **good/Comparison.stl**

#### For 12 points:

- functions with value parameters, also recursive - **good/Recursive.stl**

#### For 16 points:

- bool type - **good/Bool.stl**, **bad/IfInt.stl**
- arithmetic, comparisons
- while, if elif else - **good/{IfElifElse.stl, IfElif.stl, IfElse.stl, If.stl}**
- functions/procedures
- print statement - **good/Print.stl**
- `++`, `--`, `+=`, `-=`, `*=`, `/=`, `%=` operators - **good/Assign.stl**
- string type, literals - **good/String.stl**
- for C++ style, `for .. in ..` - **good/{For.stl, Foreach.stl}**

#### For 20 points:

- shadowing with static binding - **warn/Shadow.stl**
- static typing - most of **bad** examples
- division by zero runtime errors - **bad/{DivZero.stl, ModZero}**
- functions returning values - **good/FacIter.stl**
- lists - **good/{List.stl, Listception.stl}**
- functions parameters - **good/{FunctionParameters.stl, PassFunction.stl}**
- returning function - **good/ReturnFunction.stl**
