IntLang Interpreter
=============

Interpreter for Intuitive Language, a challange on HR. 

## Intuitive Language description 

Sometimes it is hard to read code written in the language you are not familiar with, not unless its written in Intuitive Language. Here's the sample of a program.

```
A is 15.  
Sum is function of 2: 1, 1, 0.  
Inc is function of 1: 1, 1.  
I is 1.  
F1 is 1.  

do {10} assign I*F1 to F1 AND Inc[I] to I!  
what is I AND F1?  

what is Sum[1]?  
```
Expected output for this program would be: 
```
11
3628800
1, 1
```
Pretty straightforward, when compared with some popular languages, isn't it? Lets break down the rules here. 

Language allows:
* To declare function.
* Assign value to variable.
* Make a loop with fixed number of repetition.
* Ask about function's value.

Above program is inluded in this repository and can be found in [test scripts](test-scripts/test.intl).

### Installation

```bash
cabal install mtl
cabal install parsec
```



 ## Run 

Compile into executable with GHC
```bash
ghc src/repl.hs -o run.o
```

Then you can run one of the test-scripts included.

``` 
$./src/run test-scripts/test.intl
11
3628800
1, 1

$ ./src/run test-scripts/test5.intl
1/3, 0
-3, -1, -2
2
1
28/27
1/1000000, 10100, -10100
3/4, 0
```