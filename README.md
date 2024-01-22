# Bad C parser
A C parser and compiler based on [lalrpop](https://github.com/lalrpop/lalrpop/tree/master)
A very limited subset of C is implemented, but you can use standard C libraries.

## Grammar
For grammar, go to [c.lalrpop](src/c.lalrpop) file.

## TODO
- [x] Fill in asm emission
- [x] Extern function declaration
- [x] Function declaration handling
- [x] Error handling
- [ ] Nice error handling
- [x] Comment support
- [ ] C preprocessor support
- [ ] Optimization pass
- [ ] Scopes

# Some x86 defintions
## Registers:
- RBP - Base pointer - address of start of current stack frame
- RSP - Stack pointer - address of start top of the stack


# Resources:
- C calling convention for 64bit https://aaronbloomfield.github.io/pdr/book/x86-64bit-ccc-chapter.pdf
