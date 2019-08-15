reset:
    JMP start
start:
    JMP (start + -3) * 4
    JMP start[0]
    JMP start[1]
    JMP start << 1
    JMP start >> 1
