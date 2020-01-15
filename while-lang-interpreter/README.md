# while-lang-interpreter

Interpreter and a REPL for Intuitive Language, a challange on HR. 

# While Language  

Here you have to design an interpreter for a subset of the While language. It is a simple imperative language which only supports integer literals.
We will use similar grammar which its authors1,2,3 have used. Below is the description of grammar that we will use.
```
x, y ∈ Var (variables)
n ∈ Num (numerals/integers)
opa ∈ Opa (arithmetic operators) 
opa ::= + | - | * | /
opb ∈ Opb (boolean operators) 
opb ::= and | or
opr ∈ Opr (relational operators) 
opr ::= > | <
a ∈ AExp (arithmetic expressions) 
a ::= x | n | a1 opa a2 | ( a )
b ∈ BExp (boolean expressions) 
b ::= true | false | b1 opb b2 | a1 opr a2 | ( b )
S ∈ Stmt (statements) 
S ::= x := a | S1 ; S2 | if b then { S1 } else { S2 } | while b do { S }
```
Here all operators are left associative. Their precedence order is as follows.
- Arithmetic Operators: (*, /) > (+, -) > (>, <)
- Boolean Operators: and > or

You can safely assume that all variables have integer type and are initialized properly. All variables name will consist of only lowercase letter ('a'-'z') and it's length will not exceed 10.
Note that ";" is more like of a sequencing operator. It is used to concatenate two statements. That's why there will be no ";" at the end of block of statements.
All divisions are integers divisions, that is, a/b = floor(a/b). Intermediate values of any variable will always be in range [0, 2*1018].
All test cases are valid programs. All of them will execute no more than 106 operations. All operators and operand will be separated by at least one white space.
Input 
Input will be the multiline While program. You have to read it to the end of file.
Output 
At the end of program, you have to print each variable's name and its value, in different lines, sorted by the lexicographical order of name.

Above program is inluded in this repository and can be found in [test scripts](test-scripts/test.wl).

## Installation

### Requirements
[Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) installed. 

### Build 
```
$ cd while-lang-interpreter
$ stack build
```



 ## Run 

Once the project is built you can run one of the test-scripts included.

``` 
$stack exec while-lang-interpreter-exe test-scripts/test.wl
cur 0
fact 3628800
mod 1000000007
val 10

$stack exec while-lang-interpreter-exe test-scripts/test3.wl
a 10
b 100
max 100
min 10
```


### REPL session
Example of using REPL to calculate Factorial of 15 in While Language: 
```
$ stack exec while-lang-interpreter-exe                                    
WhileLang>>x := 15
x 15
WhileLang>>y := 1
x 15
y 1
WhileLang>>ENV
x 15
y 1
WhileLang>> while (x > 1) do { y := y * x; x := x -1 }
x 1
y 1307674368000
WhileLang>>
```