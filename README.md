#Si interpreter
C-like language interpreter

BNFC grammar: Si.cf

Example programs: good and bad folders

#How to run
```$ make``` (alex, happy and ghc required)

```$ ./interpreter < <file with code>```

#Functionality
- types: int, bool, char, string, void (the last one can contain only one value that means nothing)
- basic C instructions: arithmetic, comaprisions, if, while, for, += and ++ operators, char<->int conversions
- functions (incl. anonymous functions, passing arguments by value and by reference, recursion)
- print, println
- handling runtime errors: division by zero, accessing a non-existing index of an array
- structs
- multidimensional arrays
- static type checking before the execution

Default values:
- int - 0
- bool - false
- char - char with index 0
- string - ""
- int[] - empty array of ints

The way the language works slightly differs from how C works, look at the grammar and code examples to know more.
