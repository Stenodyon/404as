const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const SegmentedList = std.SegmentedList;

usingnamespace @import("tokenizer.zig");
usingnamespace @import("architecture.zig");
usingnamespace @import("expressions.zig");
usingnamespace @import("utils.zig");

pub const LabelDecl = struct {
    loc: SourceLoc,
    label: []const u8,
};

pub const ParsedInstruction = struct {
    const Operands = union(enum) {
        Expression: *Expression,
        RegisterPair: RegisterPair,
    };

    id: @TagType(Instruction),
    operands: ?Operands,
};

pub const InstrStmt = struct {
    loc: SourceLoc,
    instruction: ParsedInstruction,
};

const ParsedLine = struct {
    label: ?LabelDecl,
    instruction: ?InstrStmt,
};

pub const Statement = union(enum) {
    Label: LabelDecl,
    Instruction: InstrStmt,
};

pub const ParseResult = struct {
    statements: []Statement,
    expressions: SegmentedList(Expression, 0),

    pub fn deinit(self: *ParseResult) void {
        self.expressions.allocator.free(self.statements);
        self.expressions.deinit();
    }
};

const ParseContext = struct {
    tokens: []const Token,
    current_token: usize,
    expressions: SegmentedList(Expression, 0),

    fn init(allocator: *Allocator, tokens: []const Token) ParseContext {
        return ParseContext{
            .tokens = tokens,
            .current_token = 0,
            .expressions = SegmentedList(Expression, 0).init(allocator),
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

    fn parse_address(self: *ParseContext) !*Expression {
        const token = self.eat_token() orelse {
            std.debug.warn("Unexpected end of file\n");
            std.process.exit(1);
        };
        if (token.id == .Number) {
            const expr = Expression{
                .Literal = ExprLiteral{
                    .loc = token.loc,
                    .value = token.number_value,
                },
            };
            try self.expressions.push(expr);
            return self.expressions.at(self.expressions.count() - 1);
        } else if (token.id == .Symbol) {
            const expr = Expression{
                .Label = ExprLabel{
                    .loc = token.loc,
                    .name = token.contents,
                },
            };
            try self.expressions.push(expr);
            return self.expressions.at(self.expressions.count() - 1);
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

    fn parse_instruction(self: *ParseContext) !?InstrStmt {
        const symbol = self.eat_token_if(.Symbol) orelse return null;
        const instr: ParsedInstruction = blk: {
            if (streqi("NOP", symbol.contents)) {
                break :blk ParsedInstruction{
                    .id = .NOP,
                    .operands = null,
                };
            } else if (streqi("LDI", symbol.contents)) {
                const value = self.expect_token(.Number);
                const lit = Expression{
                    .Literal = ExprLiteral{
                        .loc = symbol.loc,
                        .value = value.number_value,
                    },
                };
                try self.expressions.push(lit);
                const expr = self.expressions.at(self.expressions.count() - 1);
                break :blk ParsedInstruction{
                    .id = .LDI,
                    .operands = ParsedInstruction.Operands{ .Expression = expr },
                };
            } else if (streqi("LOD", symbol.contents)) {
                break :blk ParsedInstruction{
                    .id = .LOD,
                    .operands = null,
                };
            } else if (streqi("STR", symbol.contents)) {
                break :blk ParsedInstruction{
                    .id = .STR,
                    .operands = null,
                };
            } else if (streqi("SAR", symbol.contents)) {
                break :blk ParsedInstruction{
                    .id = .SAR,
                    .operands = null,
                };
            } else if (streqi("SAP", symbol.contents)) {
                break :blk ParsedInstruction{
                    .id = .SAP,
                    .operands = null,
                };
            } else if (streqi("MOV", symbol.contents)) {
                const reg_pair = self.parse_register_pair();
                break :blk ParsedInstruction{
                    .id = .MOV,
                    .operands = ParsedInstruction.Operands{ .RegisterPair = reg_pair },
                };
            } else if (streqi("CLC", symbol.contents)) {
                break :blk ParsedInstruction{
                    .id = .CLC,
                    .operands = null,
                };
            } else if (streqi("JMP", symbol.contents)) {
                const address_expr = try self.parse_address();
                break :blk ParsedInstruction{
                    .id = .JMP,
                    .operands = ParsedInstruction.Operands{ .Expression = address_expr },
                };
            } else if (streqi("RJP", symbol.contents)) {
                break :blk ParsedInstruction{
                    .id = .RJP,
                    .operands = null,
                };
            } else if (streqi("JZ", symbol.contents)) {
                const address_expr = try self.parse_address();
                break :blk ParsedInstruction{
                    .id = .JZ,
                    .operands = ParsedInstruction.Operands{ .Expression = address_expr },
                };
            } else if (streqi("JC", symbol.contents)) {
                const address_expr = try self.parse_address();
                break :blk ParsedInstruction{
                    .id = .JC,
                    .operands = ParsedInstruction.Operands{ .Expression = address_expr },
                };
            } else if (streqi("ADD", symbol.contents)) {
                const reg_pair = self.parse_register_pair();
                break :blk ParsedInstruction{
                    .id = .ADD,
                    .operands = ParsedInstruction.Operands{ .RegisterPair = reg_pair },
                };
            } else if (streqi("NAND", symbol.contents)) {
                const reg_pair = self.parse_register_pair();
                break :blk ParsedInstruction{
                    .id = .NAND,
                    .operands = ParsedInstruction.Operands{ .RegisterPair = reg_pair },
                };
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

    fn parse_line(self: *ParseContext) !?ParsedLine {
        if (!self.has_tokens())
            return null;

        while (self.eat_token_if(.Comment)) |_| {
            _ = self.expect_token(.Newline);
        }

        var line = ParsedLine{ .label = null, .instruction = null };
        line.label = self.parse_label_decl();
        line.instruction = try self.parse_instruction();

        _ = self.eat_token_if(.Comment);
        _ = self.expect_token(.Newline);

        return line;
    }

    pub fn parse(self: *ParseContext, allocator: *Allocator) !ParseResult {
        var statements = ArrayList(Statement).init(allocator);

        while (try self.parse_line()) |line| {
            if (line.label) |label| {
                try statements.append(Statement{ .Label = label });
            }
            if (line.instruction) |instruction| {
                try statements.append(Statement{ .Instruction = instruction });
            }
        }

        return ParseResult{
            .statements = statements.toOwnedSlice(),
            .expressions = self.expressions,
        };
    }
};

pub fn parse(
    allocator: *Allocator,
    filename: []const u8,
    source: []const u8,
) !ParseResult {
    const tokens = try tokenize(allocator, filename, source);
    defer allocator.free(tokens);

    var pc = ParseContext.init(allocator, tokens);
    const result = try pc.parse(allocator);

    return result;
}
