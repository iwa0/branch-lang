Abstract Syntax Tree
---

```
program <- statement*
statement <-
    declare_statement
    / if_statement
    / while_statement
    / '{' statement* '}'
    / assign_statement semi
    / expression semi

declare_statement <- ('int' / 'bool') ident '=' expression
if_statement <- 'if' expression statement ('else' statement)?
while_statement <- 'while' expression statement
assign_statement <- expression '=' expression

expression <-
    ident
    / 'true'
    / 'false'
    / number
    / '(' unary_op expression ')'
    / '(' expression binary_op expression ')'

unary_op <-
    '-'
    / '!'
binary_op <-
    '='
    / '<'
    / '>'
    / '+'
    / '-'
    / '*'
    / 'and'
    / 'or'
```

Pseudo-IR
---

```
program = (basic-block? instruction)*

basic-block = -- leader-instruction-label '[' predecessor-instructions ']'
leader-instruction-label = label
predecessor-instructions = label-list?

instruction = label :: instruction-name instruction-arguments : type
instruction-arguments = label-list?

label = (ident '.') '%' number
label-list = label ((', ') label)+
.
.
.
```
