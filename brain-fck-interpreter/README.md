# Brain-f__k Interpreter

## Installation 
### Requirements 
You need to have [Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) installed.

### Build 
```bash
$ cd brain-fck-interpreter/
$ stack build
```

To also expose type information for tooling like VS Code Haskero plugin, run following.
```bash
stack build intero --copy-compiler-tool
```


### REPL session 
```haskell
λ> loadFile "test-cases/test1.bfc"
Hello World!
λ> loadFile "test-cases/test2.bfc"
bcdwxy
λ> loadFile "test-cases/test3.bfc"
sp
PROCESS TIME OUT. KILLED!!!
λ> loadFile "test-cases/test13.bfc"
Cp
PROCESS TIME OUT. KILLED!!!

```
