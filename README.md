# 404 Assembler

This assembler was created for my fictional CPU, the 404.

## Syntax

```asm
; Comments start with a semicolon
loop:        ; labels are defined with a colon

    LDI 0    ; the assembler supports decimal, hex (0x..) and binary (0b..) literals
    MOV A, B ; moves A to B
    JMP loop ; branching can use a literal value or a label
```
