const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;

usingnamespace @import("architecture.zig");
usingnamespace @import("parser.zig");
usingnamespace @import("expressions.zig");
usingnamespace @import("utils.zig");

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
            self.data.items[byte_id] = @intCast(u8, (value & 0xFF0) >> 4);
            self.data.items[byte_id + 1] &= 0x0F;
            self.data.items[byte_id + 1] |= @intCast(u8, (value & 0x00F) << 4);
        } else { // first nibble is lower
            self.data.items[byte_id] &= 0xF0;
            self.data.items[byte_id] |= @intCast(u8, value >> 8);
            self.data.items[byte_id + 1] = @intCast(u8, value & 0xFF);
        }
    }

    pub fn overwrite_nibble(self: *AsmBuffer, value: u4, location: usize) void {
        const byte_id = location >> 1;
        if (location & 1 == 0) { // upper nibble
            self.data.items[byte_id] &= 0xF;
            self.data.items[byte_id] |= @intCast(u8, value) << 4;
        } else {
            self.data.items[byte_id] &= 0xF0;
            self.data.items[byte_id] |= @intCast(u8, value);
        }
    }
};

test "AsmBuffer.write_address upper" {
    var buffer = AsmBuffer.init(std.debug.global_allocator);
    {
        var i: usize = 4;
        while (i > 0) : (i -= 1) {
            try buffer.write_nibble(0);
        }
    }

    buffer.write_address(0x123, 0);
    if (buffer.data.len != 2) {
        std.debug.panic("buffer.data.len = {} != 2\n", buffer.data.len);
    }
    if (buffer.data.at(0) != 0x12 or buffer.data.at(1) != 0x30) {
        std.debug.panic(
            "buffer was [{}, {}], expected [18, 48]\n",
            buffer.data.at(0),
            buffer.data.at(1),
        );
    }
}

test "AsmBuffer.write_address lower" {
    var buffer = AsmBuffer.init(std.debug.global_allocator);
    {
        var i: usize = 4;
        while (i > 0) : (i -= 1) {
            try buffer.write_nibble(0);
        }
    }

    buffer.write_address(0x123, 1);
    if (buffer.data.len != 2) {
        std.debug.panic("buffer.data.len = {} != 2\n", buffer.data.len);
    }
    if (buffer.data.at(0) != 0x01 or buffer.data.at(1) != 0x23) {
        std.debug.panic(
            "buffer was [{}, {}], expected [1, 35]\n",
            buffer.data.at(0),
            buffer.data.at(1),
        );
    }
}

test "AsmBuffer.overwrite_nibble" {
    var buffer = AsmBuffer.init(std.debug.global_allocator);
    {
        var i: usize = 4;
        while (i > 0) : (i -= 1) {
            try buffer.write_nibble(0);
        }
    }
    buffer.overwrite_nibble(0xF, 0);
    buffer.overwrite_nibble(0xF, 3);

    assert(buffer.data.len == 2);
    for (buffer.data.toSlice()) |byte| {
        std.debug.warn("{X:2} ", byte);
    }
    assert(buffer.data.at(0) == 0xF0);
    assert(buffer.data.at(1) == 0x0F);
}

fn get_opcode(instruction: @TagType(Instruction)) u8 {
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

const Assemble = struct {
    const LabelLocMap = AutoHashMap([]const u8, usize);
    const ExprEvalContext = struct {
        expr: *Expression,
        size: usize, // in nibbles
    };

    buffer: AsmBuffer,
    label_locations: LabelLocMap,
    label_to_fill: AutoHashMap(usize, LabelDecl),
    expr_to_eval: AutoHashMap(usize, ExprEvalContext),

    pub fn init(allocator: *Allocator) Assemble {
        return Assemble{
            .buffer = AsmBuffer.init(allocator),
            .label_locations = LabelLocMap.init(allocator),
            .label_to_fill = AutoHashMap(usize, LabelDecl).init(allocator),
            .expr_to_eval = AutoHashMap(usize, ExprEvalContext).init(allocator),
        };
    }

    pub fn return_assembly(self: *Assemble) []u8 {
        self.label_locations.deinit();
        self.label_to_fill.deinit();
        self.expr_to_eval.deinit();
        return self.buffer.data.toOwnedSlice();
    }

    pub fn mark_label(self: *Assemble, name: []const u8) !void {
        _ = try self.label_locations.put(name, self.buffer.location);
    }

    pub fn defer_expr_eval(
        self: *Assemble,
        expr: *Expression,
        size: usize,
    ) !void {
        _ = try self.expr_to_eval.put(self.buffer.location, ExprEvalContext{
            .expr = expr,
            .size = size,
        });

        var i: usize = 0;
        while (i < size) : (i += 1) {
            try self.buffer.write_nibble(0);
        }
    }

    pub fn eval_expressions(self: *Assemble) void {
        var iter = self.expr_to_eval.iterator();
        while (iter.next()) |entry| {
            const expr = entry.value.expr;
            const size = entry.value.size;
            const max_value = usize(1) << @intCast(u6, size * 4);
            const location = entry.key;
            var value = expr.evaluate(&self.label_locations, location);
            if (value >= max_value) {
                warn(
                    expr.source_loc(),
                    "value ({}) is larger than {}, it will be truncated\n",
                    value,
                    max_value,
                );
            }
            value &= max_value - 1;
            const bytes = @bitCast([@sizeOf(@typeOf(value))]u8, value);
            var i = size;
            while (i > 0) : (i -= 1) {
                const k = i - 1;
                const byte = bytes[k >> 1];
                const nibble = if (k & 1 == 0) byte & 0xF else (byte & 0xF0) >> 4;
                self.buffer.overwrite_nibble(
                    @intCast(u4, nibble),
                    location + size - i,
                );
            }
        }
    }

    pub fn emit_immediate(self: *Assemble, expr: *Expression) !void {
        try self.defer_expr_eval(expr, 1);
    }

    pub fn emit_address(self: *Assemble, expr: *Expression) !void {
        try self.defer_expr_eval(expr, 3);
    }

    pub fn emit_register_pair(self: *Assemble, reg_pair: RegisterPair) !void {
        const AA: u8 = @enumToInt(reg_pair.a);
        const BB: u8 = @enumToInt(reg_pair.b);
        const value = AA << 2 | BB;
        try self.buffer.write_nibble(value);
    }

    pub fn emit_instruction(self: *Assemble, instr_stmt: InstrStmt) !void {
        const instruction = instr_stmt.instruction;
        try self.buffer.write_nibble(get_opcode(instruction.id));
        switch (instruction.id) {
            .LDI => {
                const expr = switch (instruction.operands orelse unreachable) {
                    .Expression => |e| e,
                    else => unreachable,
                };
                try self.emit_immediate(expr);
            },
            .MOV, .ADD, .NAND => {
                const reg_pair = switch (instruction.operands orelse unreachable) {
                    .RegisterPair => |register_pair| register_pair,
                    else => unreachable,
                };
                try self.emit_register_pair(reg_pair);
            },
            .JMP, .JZ, .JC => {
                const expr = switch (instruction.operands orelse unreachable) {
                    .Expression => |e| e,
                    else => unreachable,
                };
                try self.emit_address(expr);
            },
            else => {},
        }
    }
};

// test "Assemble.emit_literal" {
//     var a = Assemble.init(std.debug.global_allocator);
//     try a.emit_immediate(0x1);
//     try a.emit_immediate(0xF);
//     try a.emit_immediate(0xF);
//     try a.emit_immediate(0xA);
//
//     const assembly = a.return_assembly();
//
//     assert(assembly.len == 2);
//     assert(assembly[0] == 0x1F);
//     assert(assembly[1] == 0xFA);
// }
//
// test "Assemble.emit_literal_address" {
//     var a = Assemble.init(std.debug.global_allocator);
//     try a.emit_literal_address(0x123);
//     try a.emit_literal_address(0x456);
//
//     const assembly = a.return_assembly();
//
//     assert(assembly.len == 3);
//     assert(assembly[0] == 0x12);
//     assert(assembly[1] == 0x34);
//     assert(assembly[2] == 0x56);
// }
//
// test "Assemble.emit_label" {
//     var a = Assemble.init(std.debug.global_allocator);
//     _ = try a.label_locations.put("test_label", 0x123);
//
//     const decl = LabelDecl{
//         .loc = SourceLoc.init("test"),
//         .label = "test_label",
//     };
//     try a.emit_label(decl);
//     try a.emit_label(decl);
//     a.fill_labels();
//
//     const assembly = a.return_assembly();
//     assert(assembly.len == 3);
//     assert(assembly[0] == 0x12);
//     assert(assembly[1] == 0x31);
//     assert(assembly[2] == 0x23);
// }
//
// test "Assemble.emit_register_pair" {
//     var a = Assemble.init(std.debug.global_allocator);
//     const pair1 = RegisterPair{ .a = .A, .b = .B };
//     const pair2 = RegisterPair{ .a = .C, .b = .D };
//     try a.emit_register_pair(pair1);
//     try a.emit_register_pair(pair2);
//
//     const assembly = a.return_assembly();
//     assert(assembly.len == 1);
//     assert(assembly[0] == 0x1B);
// }

pub fn assemble(
    allocator: *Allocator,
    filename: []const u8,
    source: []const u8,
) ![]u8 {
    var parse_result = try parse(allocator, filename, source);
    defer parse_result.deinit();
    var a = Assemble.init(allocator);

    for (parse_result.statements) |statement| {
        switch (statement) {
            .Label => |label_decl| try a.mark_label(label_decl.label),
            .Instruction => |instr_stmt| try a.emit_instruction(instr_stmt),
        }
    }

    a.eval_expressions();
    const assembly = a.return_assembly();
    if (assembly.len > 256) {
        std.debug.warn("warning: output assembly is over 256B\n");
    }
    return assembly;
}
