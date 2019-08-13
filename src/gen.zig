const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;

usingnamespace @import("parser.zig");
const SourceLoc = @import("tokenizer.zig").SourceLoc;

pub const Register = enum {
    A,
    B,
    C,
    D,
};

pub const Address = union(enum) {
    Value: usize,
    Label: LabelDecl,
};

pub const RegisterPair = struct {
    a: Register,
    b: Register,
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

fn warn(loc: SourceLoc, comptime fmt: []const u8, args: ...) void {
    std.debug.warn("{}:{}:{} warning: ", loc.filename, loc.row, loc.col);
    std.debug.warn(fmt, args);
}

const AsmBuffer = struct {
    data: ArrayList(u8),
    location: usize,

    pub fn init(allocator: *Allocator) AsmBuffer {
        return AsmBuffer{
            .data = ArrayList(u8).init(allocator),
            .location = 0,
        };
    }

    pub fn write_nibble(self: *AsmBuffer, value: u8) !void {
        var shifted = value;
        if (self.location % 2 == 0) { // upper nibble
            try self.data.append(0);
            shifted <<= 4;
        }
        self.data.items[self.data.len - 1] |= shifted;
        self.location += 1;
    }

    pub fn write_address(
        self: *AsmBuffer,
        value: usize,
        location: usize,
    ) void {
        const byte_id = location >> 1;
        if (location % 2 == 0) { // first nibble is upper
            self.data.items[byte_id] = @intCast(u8, value >> 4);
            const low = (value & 0x00F) << 4;
            self.data.items[byte_id + 1] &= 0x0F;
            self.data.items[byte_id + 1] |= @intCast(u8, value);
        } else { // first nibble is lower
            self.data.items[byte_id] &= 0xF0;
            self.data.items[byte_id] |= @intCast(u8, value >> 8);
            self.data.items[byte_id + 1] = @intCast(u8, value & 0xFF);
        }
    }
};

fn get_opcode(instruction: Instruction) u8 {
    switch (instruction) {
        .NOP => return 0b0000,
        .LDI => return 0b0001,
        .LOD => return 0b0010,
        .STR => return 0b0011,
        .SAR => return 0b0100,
        .SAP => return 0b0101,
        .MOV => return 0b0110,
        .CLC => return 0b0111,
        .JMP => return 0b1000,
        .RJP => return 0b1001,
        .JZ => return 0b1010,
        .JC => return 0b1011,
        .ADD => return 0b1100,
        .NAND => return 0b1101,
    }
}

pub fn assemble(
    allocator: *Allocator,
    filename: []const u8,
    source: []const u8,
) ![]u8 {
    const statements = try parse(allocator, filename, source);
    defer allocator.free(statements);
    var buffer = AsmBuffer.init(allocator);
    var label_locations = AutoHashMap([]const u8, usize).init(allocator);
    var label_to_fill = AutoHashMap(usize, LabelDecl).init(allocator);
    defer {
        label_to_fill.deinit();
        label_locations.deinit();
    }

    for (statements) |statement| {
        if (statement.label) |label_decl| {
            _ = try label_locations.put(label_decl.label, buffer.location);
        }
        if (statement.instruction) |instr_stmt| {
            const instruction = instr_stmt.instruction;
            try buffer.write_nibble(get_opcode(instruction));
            switch (instruction) {
                .LDI => |value| {
                    if (value >= (1 << 4)) {
                        warn(
                            instr_stmt.loc,
                            "value is bigger than 15, it will be truncated\n",
                        );
                    }
                    try buffer.write_nibble(@intCast(u8, value & 0xF));
                },
                .MOV, .ADD, .NAND => |reg_pair| {
                    const AA: u8 = @enumToInt(reg_pair.a);
                    const BB: u8 = @enumToInt(reg_pair.b);
                    const value = AA << 2 | BB;
                    try buffer.write_nibble(value);
                },
                .JMP, .JZ, .JC => |address| {
                    switch (address) {
                        .Value => |value| {
                            const A2 = value >> 8;
                            const A1 = (value & 0xF0) >> 4;
                            const A0 = value & 0xF;
                            try buffer.write_nibble(@intCast(u8, A2));
                            try buffer.write_nibble(@intCast(u8, A1));
                            try buffer.write_nibble(@intCast(u8, A0));
                        },
                        .Label => |label_decl| {
                            _ = try label_to_fill.put(
                                buffer.location,
                                label_decl,
                            );
                            try buffer.write_nibble(0);
                            try buffer.write_nibble(0);
                            try buffer.write_nibble(0);
                        },
                    }
                },
                else => {},
            }
        }
    }

    var fill_iterator = label_to_fill.iterator();
    while (fill_iterator.next()) |entry| {
        const fill_loc = entry.key;
        const label_decl = entry.value;
        const loc_entry = label_locations.get(label_decl.label) orelse {
            fail(label_decl.loc, "undeclared label \"{}\"\n", label_decl.label);
        };
        buffer.write_address(loc_entry.value, fill_loc);
    }

    return buffer.data.toOwnedSlice();
}
