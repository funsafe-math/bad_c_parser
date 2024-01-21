# Bad C parser
A C parser based on [lalrpop](https://github.com/lalrpop/lalrpop/tree/master)
## Grammar
For grammar, go to [c.lalrpop](src/c.lalrpop) file.

## TODO
- [ ] Fill in asm emission
- [ ] Extern function declaration
- [ ] Function declaration handling
- [ ] Error handling
- [ ] Nice error handling
- [ ] Comment support
- [ ] C preprocessor support
- [ ] Optimization pass
- [ ] Scopres

# Some x86 definitons
## Registers:
- RBP - Base pointer - address of start of current stack frame
- RSP - Stack pointer - address of start top of the stack


# Resources:
- C calling convention for 64bit https://aaronbloomfield.github.io/pdr/book/x86-64bit-ccc-chapter.pdf
