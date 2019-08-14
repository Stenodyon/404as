const LabelDecl = @import("parser.zig").LabelDecl;

pub const Register = enum {
    A,
    B,
    C,
    D,
};

pub const RegisterPair = struct {
    a: Register,
    b: Register,
};

pub const Address = union(enum) {
    Value: usize,
    Label: LabelDecl,
};

pub const nameof_Instruction = [_][]const u8{
    "NOP",
    "LDI",
    "LOD",
    "STR",
    "SAR",
    "SAP",
    "MOV",
    "CLC",
    "JMP",
    "RJP",
    "JZ",
    "JC",
    "ADD",
    "NAND",
};

pub const Instruction = union(enum) {
    NOP,
    LDI: usize,
    LOD,
    STR,
    SAR,
    SAP,
    MOV: RegisterPair,
    CLC,
    JMP: Address,
    RJP,
    JZ: Address,
    JC: Address,
    ADD: RegisterPair,
    NAND: RegisterPair,
};
