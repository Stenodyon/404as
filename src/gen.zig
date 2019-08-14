const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;

usingnamespace @import("architecture.zig");
usingnamespace @import("parser.zig");
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

const Assemble = struct {
    const LabelLocMap = AutoHashMap([]const u8, usize);

    buffer: AsmBuffer,
    label_locations: LabelLocMap,
    label_to_fill: AutoHashMap(usize, LabelDecl),

    pub fn init(allocator: *Allocator) Assemble {
        return Assemble{
            .buffer = AsmBuffer.init(allocator),
            .label_locations = LabelLocMap.init(allocator),
            .label_to_fill = AutoHashMap(usize, LabelDecl).init(allocator),
        };
    }

    pub fn return_assembly(self: *Assemble) []u8 {
        self.label_locations.deinit();
        self.label_to_fill.deinit();
        return self.buffer.data.toOwnedSlice();
    }

    pub fn mark_label(self: *Assemble, name: []const u8) !void {
        _ = try self.label_locations.put(name, self.buffer.location);
    }

    pub fn fill_labels(self: *Assemble) void {
        var fill_iterator = self.label_to_fill.iterator();
        while (fill_iterator.next()) |entry| {
            const fill_loc = entry.key;
            const label_decl = entry.value;
            const loc_entry = self.label_locations.get(label_decl.label) orelse {
                fail(label_decl.loc, "undeclared label \"{}\"\n", label_decl.label);
            };
            self.buffer.write_address(loc_entry.value, fill_loc);
        }
    }

    pub fn emit_immediate(self: *Assemble, value: usize) !void {
        try self.buffer.write_nibble(@intCast(u8, value & 0xF));
    }

    pub fn emit_literal_address(self: *Assemble, address: usize) !void {
        const A2 = address >> 8;
        const A1 = (address & 0xF0) >> 4;
        const A0 = address & 0xF;
        try self.buffer.write_nibble(@intCast(u8, A2));
        try self.buffer.write_nibble(@intCast(u8, A1));
        try self.buffer.write_nibble(@intCast(u8, A0));
    }

    pub fn emit_label(self: *Assemble, label: LabelDecl) !void {
        _ = try self.label_to_fill.put(self.buffer.location, label);
        try self.buffer.write_nibble(0);
        try self.buffer.write_nibble(0);
        try self.buffer.write_nibble(0);
    }

    pub fn emit_address(self: *Assemble, address: Address) !void {
        switch (address) {
            .Value => |value| try self.emit_literal_address(value),
            .Label => |label_decl| try self.emit_label(label_decl),
        }
    }

    pub fn emit_register_pair(self: *Assemble, reg_pair: RegisterPair) !void {
        const AA: u8 = @enumToInt(reg_pair.a);
        const BB: u8 = @enumToInt(reg_pair.b);
        const value = AA << 2 | BB;
        try self.buffer.write_nibble(value);
    }

    pub fn emit_instruction(self: *Assemble, instr_stmt: InstrStmt) !void {
        const instruction = instr_stmt.instruction;
        try self.buffer.write_nibble(get_opcode(instruction));
        switch (instruction) {
            .LDI => |value| {
                if (value >= (1 << 4)) {
                    warn(
                        instr_stmt.loc,
                        "value ({}) is bigger than 15, it will be truncated\n",
                        value,
                    );
                }
                try self.emit_immediate(value);
            },
            .MOV, .ADD, .NAND => |reg_pair| try self.emit_register_pair(reg_pair),
            .JMP, .JZ, .JC => |address| try self.emit_address(address),
            else => {},
        }
    }
};

pub fn assemble(
    allocator: *Allocator,
    filename: []const u8,
    source: []const u8,
) ![]u8 {
    const statements = try parse(allocator, filename, source);
    defer allocator.free(statements);
    var a = Assemble.init(allocator);

    for (statements) |statement| {
        switch (statement) {
            .Label => |label_decl| try a.mark_label(label_decl.label),
            .Instruction => |instr_stmt| try a.emit_instruction(instr_stmt),
        }
    }

    a.fill_labels();
    const assembly = a.return_assembly();
    if (assembly.len > 256) {
        std.debug.warn("warning: output assembly is over 256B\n");
    }
    return assembly;
}
