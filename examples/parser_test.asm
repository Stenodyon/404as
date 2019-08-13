; Parse test
    LDI 0
    MOV A, B ; clear registers
    MOV B, C
    MOV C, D
main: JMP main

reset: JMP 0
    
