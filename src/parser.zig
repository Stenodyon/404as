const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

usingnamespace @import("tokenizer.zig");
usingnamespace @import("gen.zig");

fn streqi(comptime base: []const u8, b: []const u8) bool {
    if (base.len != b.len) return false;
    var i: usize = 0;
    while (i < base.len) : (i += 1) {
        if (base[i] != std.ascii.toUpper(b[i])) {
            return false;
        }
    }
    return true;
}

pub fn fail(loc: SourceLoc, comptime fmt: []const u8, args: ...) noreturn {
    std.debug.warn("{}:{}:{} error: ", loc.filename, loc.row, loc.col);
    std.debug.warn(fmt, args);
    std.process.exit(1);
}

pub const LabelDecl = struct {
    loc: SourceLoc,
    label: []const u8,
};

pub const InstrStmt = struct {
    loc: SourceLoc,
    instruction: Instruction,
};

const ParsedLine = struct {
    label: ?LabelDecl,
    instruction: ?InstrStmt,
};

pub const Statement = union(enum) {
    Label: LabelDecl,
    Instruction: InstrStmt,
};

const ParseContext = struct {
    tokens: []const Token,
    current_token: usize,

    fn init(allocator: *Allocator, tokens: []const Token) ParseContext {
        return ParseContext{
            .tokens = tokens,
            .current_token = 0,
        };
    }

    fn has_tokens(self: *ParseContext) bool {
        return self.current_token < self.tokens.len;
    }

    fn unexpected_token(
        self: *ParseContext,
        expected: TokenId,
        found: *const Token,
    ) noreturn {
        std.debug.warn(
            "{}:{}:{} error: expected {}, found {}\n",
            found.loc.filename,
            found.loc.row,
            found.loc.col,
            expected.to_string(),
            found.id.to_string(),
        );
        std.process.exit(1);
    }

    fn peek_token(self: *ParseContext) ?*const Token {
        if (self.current_token < self.tokens.len) {
            return &self.tokens[self.current_token];
        }
        return null;
    }

    fn eat_token(self: *ParseContext) ?*const Token {
        if (self.current_token < self.tokens.len) {
            const token = &self.tokens[self.current_token];
            self.current_token += 1;
            return token;
        }
        return null;
    }

    fn eat_token_if(self: *ParseContext, id: TokenId) ?*const Token {
        if (self.peek_token()) |token| {
            if (token.id == id)
                return self.eat_token();
            return null;
        } else {
            return null;
        }
    }

    fn expect_token(self: *ParseContext, id: TokenId) *const Token {
        if (self.eat_token()) |token| {
            if (token.id == id)
                return token;
            self.unexpected_token(id, token);
        } else {
            std.debug.warn("Unexpected end of file\n");
            std.process.exit(1);
        }
    }

    fn parse_register(self: *ParseContext) Register {
        const symbol = self.expect_token(.Symbol);
        if (streqi("A", symbol.contents)) {
            return .A;
        } else if (streqi("B", symbol.contents)) {
            return .B;
        } else if (streqi("C", symbol.contents)) {
            return .C;
        } else if (streqi("D", symbol.contents)) {
            return .D;
        }
        fail(symbol.loc, "expected register, found {}\n", symbol.contents);
    }

    fn parse_register_pair(self: *ParseContext) RegisterPair {
        const reg_a = self.parse_register();
        _ = self.expect_token(.Comma);
        const reg_b = self.parse_register();
        return RegisterPair{ .a = reg_a, .b = reg_b };
    }

    fn parse_address(self: *ParseContext) Address {
        const token = self.eat_token() orelse {
            std.debug.warn("Unexpected end of file\n");
            std.process.exit(1);
        };
        if (token.id == .Number) {
            return Address{ .Value = token.number_value };
        } else if (token.id == .Symbol) {
            return Address{
                .Label = LabelDecl{
                    .loc = token.loc,
                    .label = token.contents,
                },
            };
        }
        fail(
            token.loc,
            "expected NUMBER or LABEL, found {}\n",
            token.id.to_string(),
        );
    }

    fn parse_label_decl(self: *ParseContext) ?LabelDecl {
        const initial_loc = self.current_token;
        const symbol = self.eat_token_if(.Symbol) orelse return null;
        _ = self.eat_token_if(.Colon) orelse {
            self.current_token = initial_loc;
            return null;
        };
        return LabelDecl{ .loc = symbol.loc, .label = symbol.contents };
    }

    fn parse_instruction(self: *ParseContext) ?InstrStmt {
        const symbol = self.eat_token_if(.Symbol) orelse return null;
        const instr = blk: {
            if (streqi("NOP", symbol.contents)) {
                break :blk Instruction{ .NOP = {} };
            } else if (streqi("LDI", symbol.contents)) {
                const value = self.expect_token(.Number);
                break :blk Instruction{ .LDI = symbol.number_value };
            } else if (streqi("LOD", symbol.contents)) {
                break :blk Instruction{ .LOD = {} };
            } else if (streqi("STR", symbol.contents)) {
                break :blk Instruction{ .STR = {} };
            } else if (streqi("SAR", symbol.contents)) {
                break :blk Instruction{ .SAR = {} };
            } else if (streqi("SAP", symbol.contents)) {
                break :blk Instruction{ .SAP = {} };
            } else if (streqi("MOV", symbol.contents)) {
                const reg_pair = self.parse_register_pair();
                break :blk Instruction{ .MOV = reg_pair };
            } else if (streqi("CLC", symbol.contents)) {
                break :blk Instruction{ .CLC = {} };
            } else if (streqi("JMP", symbol.contents)) {
                const address = self.parse_address();
                break :blk Instruction{ .JMP = address };
            } else if (streqi("RJP", symbol.contents)) {
                break :blk Instruction{ .RJP = {} };
            } else if (streqi("JZ", symbol.contents)) {
                const address = self.parse_address();
                break :blk Instruction{ .JZ = address };
            } else if (streqi("JC", symbol.contents)) {
                const address = self.parse_address();
                break :blk Instruction{ .JC = address };
            } else if (streqi("ADD", symbol.contents)) {
                const reg_pair = self.parse_register_pair();
                break :blk Instruction{ .ADD = reg_pair };
            } else if (streqi("NAND", symbol.contents)) {
                const reg_pair = self.parse_register_pair();
                break :blk Instruction{ .NAND = reg_pair };
            } else {
                fail(
                    symbol.loc,
                    "unknown instruction \"{}\"\n",
                    @tagName(symbol.id),
                );
            }
        };
        return InstrStmt{ .loc = symbol.loc, .instruction = instr };
    }

    fn parse_line(self: *ParseContext) ?ParsedLine {
        if (!self.has_tokens())
            return null;

        while (self.eat_token_if(.Comment)) |_| {
            _ = self.expect_token(.Newline);
        }

        var line = ParsedLine{ .label = null, .instruction = null };
        line.label = self.parse_label_decl();
        line.instruction = self.parse_instruction();

        _ = self.eat_token_if(.Comment);
        _ = self.expect_token(.Newline);

        return line;
    }

    pub fn parse(self: *ParseContext, allocator: *Allocator) ![]Statement {
        var statements = ArrayList(Statement).init(allocator);

        while (self.parse_line()) |line| {
            if (line.label) |label| {
                try statements.append(Statement{ .Label = label });
            }
            if (line.instruction) |instruction| {
                try statements.append(Statement{ .Instruction = instruction });
            }
        }

        return statements.toOwnedSlice();
    }
};

pub fn parse(
    allocator: *Allocator,
    filename: []const u8,
    source: []const u8,
) ![]Statement {
    const tokens = try tokenize(allocator, filename, source);
    defer allocator.free(tokens);

    var pc = ParseContext.init(allocator, tokens);
    const statements = pc.parse(allocator);

    return statements;
}
