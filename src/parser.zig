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

    const ExprError = @typeOf(SegmentedList(Expression, 0).push).ReturnType.ErrorSet;

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
            @tagName(expected),
            @tagName(found.id),
        );
        std.process.exit(1);
    }

    fn peek_token(self: *ParseContext) ?*const Token {
        if (self.current_token < self.tokens.len) {
            return &self.tokens[self.current_token];
        }
        return null;
    }

    fn peek_token_if(self: *ParseContext, id: TokenId) ?*const Token {
        const token = self.peek_token() orelse return null;
        if (token.id == id)
            return token;
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

    fn push_expression(self: *ParseContext, expr: Expression) !*Expression {
        try self.expressions.push(expr);
        return self.expressions.at(self.expressions.count() - 1);
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

    fn parse_number(self: *ParseContext) !?*Expression {
        const token = self.eat_token_if(.Number) orelse return null;
        const expr = Expression{
            .Literal = ExprLiteral{
                .loc = token.loc,
                .value = token.number_value,
            },
        };
        try self.expressions.push(expr);
        return self.expressions.at(self.expressions.count() - 1);
    }

    fn parse_label(self: *ParseContext) !?*Expression {
        const token = self.eat_token_if(.Symbol) orelse return null;
        const expr = Expression{
            .Label = ExprLabel{
                .loc = token.loc,
                .name = token.contents,
            },
        };
        try self.expressions.push(expr);
        return self.expressions.at(self.expressions.count() - 1);
    }

    fn parse_expression(self: *ParseContext) ExprError!?*Expression {
        return try self.parse_bitwiseOR();
    }

    fn parse_bitwiseOR(self: *ParseContext) !?*Expression {
        const lhs = (try self.parse_bitwiseXOR()) orelse return null;
        const operator_token = self.eat_token_if(.Pipe) orelse return lhs;
        const rhs = (try self.parse_bitwiseXOR()) orelse {
            fail(
                operator_token.loc,
                "expected expression after '{}'\n",
                operator_token.contents,
            );
        };
        return try self.push_expression(Expression{
            .Binop = ExprBinop{
                .lhs = lhs,
                .rhs = rhs,
                .op = Binop.OR,
            },
        });
    }

    fn parse_bitwiseXOR(self: *ParseContext) !?*Expression {
        const lhs = (try self.parse_bitwiseAND()) orelse return null;
        const operator_token = self.eat_token_if(.Caret) orelse return lhs;
        const rhs = (try self.parse_bitwiseAND()) orelse {
            fail(
                operator_token.loc,
                "expected expression after '{}'\n",
                operator_token.contents,
            );
        };
        return try self.push_expression(Expression{
            .Binop = ExprBinop{
                .lhs = lhs,
                .rhs = rhs,
                .op = Binop.XOR,
            },
        });
    }

    fn parse_bitwiseAND(self: *ParseContext) !?*Expression {
        const lhs = (try self.parse_shift_expr()) orelse return null;
        const operator_token = self.eat_token_if(.Ampersand) orelse return lhs;
        const rhs = (try self.parse_shift_expr()) orelse {
            fail(
                operator_token.loc,
                "expected expression after '{}'\n",
                operator_token.contents,
            );
        };
        return try self.push_expression(Expression{
            .Binop = ExprBinop{
                .lhs = lhs,
                .rhs = rhs,
                .op = Binop.AND,
            },
        });
    }

    fn parse_shift_expr(self: *ParseContext) !?*Expression {
        const lhs = (try self.parse_addsub_expr()) orelse return null;
        const operator_token = blk: {
            if (self.eat_token_if(.ShiftLeft)) |token| break :blk token;
            if (self.eat_token_if(.ShiftRight)) |token| break :blk token;
            return lhs;
        };
        const operator: Binop = switch (operator_token.id) {
            .ShiftLeft => Binop.ShiftLeft,
            .ShiftRight => Binop.ShiftRight,
            else => unreachable,
        };
        const rhs = (try self.parse_addsub_expr()) orelse {
            fail(
                operator_token.loc,
                "expected expression after '{}'\n",
                operator_token.contents,
            );
        };
        return try self.push_expression(Expression{
            .Binop = ExprBinop{
                .lhs = lhs,
                .rhs = rhs,
                .op = operator,
            },
        });
    }

    fn parse_addsub_expr(self: *ParseContext) !?*Expression {
        const lhs = (try self.parse_muldiv_expr()) orelse return null;
        const operator_token = blk: {
            if (self.eat_token_if(.Plus)) |token| break :blk token;
            if (self.eat_token_if(.Minus)) |token| break :blk token;
            return lhs;
        };
        const operator: Binop = switch (operator_token.id) {
            .Plus => Binop.Add,
            .Minus => Binop.Sub,
            else => unreachable,
        };
        const rhs = (try self.parse_muldiv_expr()) orelse {
            fail(
                operator_token.loc,
                "expected expression after '{}'\n",
                operator_token.contents,
            );
        };
        return try self.push_expression(Expression{
            .Binop = ExprBinop{
                .lhs = lhs,
                .rhs = rhs,
                .op = operator,
            },
        });
    }

    fn parse_muldiv_expr(self: *ParseContext) !?*Expression {
        const lhs = (try self.parse_neg_expr()) orelse return null;
        const operator_token = blk: {
            if (self.eat_token_if(.Asterisk)) |token| break :blk token;
            if (self.eat_token_if(.Slash)) |token| break :blk token;
            return lhs;
        };
        const operator: Binop = switch (operator_token.id) {
            .Asterisk => Binop.Mul,
            .Slash => Binop.Div,
            else => unreachable,
        };
        const rhs = (try self.parse_neg_expr()) orelse {
            fail(
                operator_token.loc,
                "expected expression after '{}'\n",
                operator_token.contents,
            );
        };
        return try self.push_expression(Expression{
            .Binop = ExprBinop{
                .lhs = lhs,
                .rhs = rhs,
                .op = operator,
            },
        });
    }

    fn parse_neg_expr(self: *ParseContext) !?*Expression {
        const operator_token = blk: {
            if (self.eat_token_if(.Minus)) |token| break :blk token;
            if (self.eat_token_if(.Tilda)) |token| break :blk token;
            return try self.parse_access_expr();
        };
        const operator = switch (operator_token.id) {
            .Minus => Unop.Neg,
            .Tilda => Unop.NOT,
            else => unreachable,
        };
        const expr = (try self.parse_access_expr()) orelse {
            fail(
                operator_token.loc,
                "expected expression after '{}'\n",
                operator_token.contents,
            );
        };
        return try self.push_expression(Expression{
            .Unop = ExprUnop{
                .expr = expr,
                .op = operator,
            },
        });
    }

    fn parse_access_expr(self: *ParseContext) !?*Expression {
        const expr = (try self.parse_term()) orelse return null;
        const bracket = self.eat_token_if(.SquareBracketOpen) orelse {
            return expr;
        };
        const offset = self.expect_token(.Number);
        _ = self.expect_token(.SquareBracketClose);
        return try self.push_expression(Expression{
            .ArrayAccess = ExprArrayAccess{
                .expr = expr,
                .offset = offset.number_value,
            },
        });
    }

    fn parse_term(self: *ParseContext) !?*Expression {
        if (try self.parse_number()) |number| return number;
        if (try self.parse_label()) |label| return label;
        if (self.eat_token_if(.ParenOpen)) |paren| {
            const expr = (try self.parse_expression()) orelse {
                fail(
                    paren.loc,
                    "expected expression after '{}'\n",
                    paren.contents,
                );
            };
            _ = self.expect_token(.ParenClose);
            return expr;
        }
        return null;
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

    fn parse_mnemonic(self: *ParseContext) ?@TagType(Instruction) {
        const symbol = self.eat_token_if(.Symbol) orelse return null;
        if (streqi("NOP", symbol.contents)) {
            return @TagType(Instruction).NOP;
        } else if (streqi("LDI", symbol.contents)) {
            return @TagType(Instruction).LDI;
        } else if (streqi("LOD", symbol.contents)) {
            return @TagType(Instruction).LOD;
        } else if (streqi("STR", symbol.contents)) {
            return @TagType(Instruction).STR;
        } else if (streqi("SAR", symbol.contents)) {
            return @TagType(Instruction).SAR;
        } else if (streqi("SAP", symbol.contents)) {
            return @TagType(Instruction).SAP;
        } else if (streqi("MOV", symbol.contents)) {
            return @TagType(Instruction).MOV;
        } else if (streqi("CLC", symbol.contents)) {
            return @TagType(Instruction).CLC;
        } else if (streqi("JMP", symbol.contents)) {
            return @TagType(Instruction).JMP;
        } else if (streqi("RJP", symbol.contents)) {
            return @TagType(Instruction).RJP;
        } else if (streqi("JZ", symbol.contents)) {
            return @TagType(Instruction).JZ;
        } else if (streqi("JC", symbol.contents)) {
            return @TagType(Instruction).JC;
        } else if (streqi("ADD", symbol.contents)) {
            return @TagType(Instruction).ADD;
        } else if (streqi("NAND", symbol.contents)) {
            return @TagType(Instruction).NAND;
        } else {
            return null;
        }
    }

    fn parse_instruction(self: *ParseContext) !?InstrStmt {
        const symbol = self.peek_token_if(.Symbol) orelse return null;
        const mnemonic = self.parse_mnemonic() orelse {
            fail(
                symbol.loc,
                "unknown instruction \"{}\"\n",
                @tagName(symbol.id),
            );
        };
        const operands: ?ParsedInstruction.Operands = switch (mnemonic) {
            .LDI, .JMP, .JZ, .JC => blk: {
                const expr = (try self.parse_expression()) orelse {
                    fail(symbol.loc, "expected expression\n");
                };
                break :blk ParsedInstruction.Operands{ .Expression = expr };
            },
            .MOV, .ADD, .NAND => blk: {
                const reg_pair = self.parse_register_pair();
                break :blk ParsedInstruction.Operands{ .RegisterPair = reg_pair };
            },
            else => null,
        };
        const instr = ParsedInstruction{ .id = mnemonic, .operands = operands };
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
