const std = @import("std");
const Allocator = std.mem.Allocator;
const AutoHashMap = std.AutoHashMap;

usingnamespace @import("utils.zig");

pub const ExprLiteral = struct {
    loc: SourceLoc,
    value: usize,
};

pub const ExprLabel = struct {
    loc: SourceLoc,
    name: []const u8,
};

pub const ExprArrayAccess = struct {
    expr: *const Expression,
    offset: usize,
};

pub const Unop = enum {
    Neg,
    NOT,
};

pub const ExprUnop = struct {
    expr: *const Expression,
    op: Unop,
};

pub const Binop = enum {
    Add,
    Sub,
    Mul,
    Div,
    ShiftLeft,
    ShiftRight,
    AND,
    OR,
    XOR,
};

pub const ExprBinop = struct {
    rhs: *const Expression,
    lhs: *const Expression,
    op: Binop,
};

const LabelMap = AutoHashMap([]const u8, usize);

pub const Expression = union(enum) {
    ReturnValue: usize,
    Literal: ExprLiteral,
    Label: ExprLabel,
    ArrayAccess: ExprArrayAccess,
    Unop: ExprUnop,
    Binop: ExprBinop,
    CurrentLoc: SourceLoc,

    pub fn source_loc(self: *const Expression) SourceLoc {
        switch (self.*) {
            .Literal => |lit| return lit.loc,
            .Label => |label| return label.loc,
            .ArrayAccess => |aa| return aa.expr.source_loc(),
            .Unop => |unop| return unop.expr.source_loc(),
            .Binop => |binop| return binop.lhs.source_loc(),
            .CurrentLoc => |loc| return loc,
            else => unreachable,
        }
    }

    fn eval(
        self: *const Expression,
        labels: *const LabelMap,
        location: usize,
    ) Expression {
        switch (self.*) {
            .ReturnValue => return self.*,
            .Literal => |lit| return Expression{ .ReturnValue = lit.value },
            .Label => |label_expr| {
                const entry = labels.get(label_expr.name) orelse {
                    fail(
                        label_expr.loc,
                        "undeclared label \"{}\"\n",
                        label_expr.name,
                    );
                };
                return Expression{ .ReturnValue = entry.value };
            },
            .ArrayAccess => |access_expr| {
                switch (access_expr.expr.eval(labels, location)) {
                    .ReturnValue => |val| {
                        const sizeof = @sizeOf(@typeOf(val));
                        const bytes = @bitCast([sizeof]u8, val);
                        const byte_id = access_expr.offset >> 1;
                        if (access_expr.offset & 1 == 0) { // low nibble
                            const value = bytes[byte_id] & 0xF;
                            return Expression{ .ReturnValue = value };
                        } else {
                            const value = (bytes[byte_id] & 0xF0) >> 4;
                            return Expression{ .ReturnValue = value };
                        }
                    },
                    else => unreachable,
                }
            },
            .Unop => |unop_expr| {
                const value = switch (unop_expr.expr.eval(labels, location)) {
                    .ReturnValue => |val| val,
                    else => unreachable,
                };
                switch (unop_expr.op) {
                    .Neg => return Expression{ .ReturnValue = -%value },
                    .NOT => return Expression{ .ReturnValue = ~value },
                }
            },
            .Binop => |binop_expr| {
                const a = switch (binop_expr.lhs.eval(labels, location)) {
                    .ReturnValue => |val| val,
                    else => unreachable,
                };
                const b = switch (binop_expr.rhs.eval(labels, location)) {
                    .ReturnValue => |val| val,
                    else => unreachable,
                };
                switch (binop_expr.op) {
                    .Add => return Expression{ .ReturnValue = a +% b },
                    .Sub => return Expression{ .ReturnValue = a -% b },
                    .Mul => return Expression{ .ReturnValue = a * b },
                    .Div => return Expression{ .ReturnValue = a / b },
                    .ShiftLeft => return Expression{
                        .ReturnValue = a << @intCast(u6, b),
                    },
                    .ShiftRight => return Expression{
                        .ReturnValue = a >> @intCast(u6, b),
                    },
                    .AND => return Expression{ .ReturnValue = a & b },
                    .OR => return Expression{ .ReturnValue = a | b },
                    .XOR => return Expression{ .ReturnValue = a ^ b },
                }
            },
            .CurrentLoc => return Expression{ .ReturnValue = location },
        }
    }

    pub fn evaluate(
        self: *const Expression,
        labels: *const LabelMap,
        location: usize,
    ) usize {
        switch (self.eval(labels, location)) {
            .ReturnValue => |val| return val,
            else => unreachable,
        }
    }
};

test "Expression.evaluate literal" {
    const labels = LabelMap.init(std.debug.global_allocator);
    const lit = Expression{
        .Literal = ExprLiteral{
            .loc = SourceLoc.init("test"),
            .value = 0xA,
        },
    };

    const result = lit.evaluate(&labels, 0);
    assert(result == 0xA);
}

test "Expression.evaluate label" {
    var labels = LabelMap.init(std.debug.global_allocator);
    _ = try labels.put("test_label", 0x123);
    const label = Expression{
        .Label = ExprLabel{
            .loc = SourceLoc.init("test"),
            .name = "test_label",
        },
    };

    const result = label.evaluate(&labels, 0);
    assert(result == 0x123);
}

test "Expression.evaluate array access" {
    var labels = LabelMap.init(std.debug.global_allocator);
    _ = try labels.put("test_label", 0x123);
    const label = Expression{
        .Label = ExprLabel{
            .loc = SourceLoc.init("test"),
            .name = "test_label",
        },
    };
    const aa = Expression{
        .ArrayAccess = ExprArrayAccess{
            .expr = &label,
            .offset = 1,
        },
    };

    const result = aa.evaluate(&labels, 0);
    std.debug.warn("{} == {} ", result, usize(0x2));
    assert(result == 0x2);
}

test "Expression.evaluate Unop.Neg" {
    const labels = LabelMap.init(std.debug.global_allocator);
    const value = 0xABBA;
    const lit = Expression{
        .Literal = ExprLiteral{
            .loc = SourceLoc.init("test"),
            .value = value,
        },
    };
    const neg = Expression{
        .Unop = ExprUnop{
            .expr = &lit,
            .op = .Neg,
        },
    };

    const result = neg.evaluate(&labels, 0);
    std.debug.warn("{} == {} ", result, usize(0) - value);
    assert(result == usize(0) - value);
}

test "Expression.evaluate Unop.NOT" {
    const labels = LabelMap.init(std.debug.global_allocator);
    const value: usize = 0xABBA;
    const lit = Expression{
        .Literal = ExprLiteral{
            .loc = SourceLoc.init("test"),
            .value = value,
        },
    };
    const neg = Expression{
        .Unop = ExprUnop{
            .expr = &lit,
            .op = .NOT,
        },
    };

    const result = neg.evaluate(&labels, 0);
    std.debug.warn("{} == {} ", result, ~value);
    assert(result == ~value);
}

fn test_binop(op: Binop) void {
    const labels = LabelMap.init(std.debug.global_allocator);
    const value1 = 47219074;
    const value2 = 15;
    const a = Expression{
        .Literal = ExprLiteral{
            .loc = SourceLoc.init("test"),
            .value = value1,
        },
    };
    const b = Expression{
        .Literal = ExprLiteral{
            .loc = SourceLoc.init("test"),
            .value = value2,
        },
    };
    const binop = Expression{
        .Binop = ExprBinop{
            .rhs = &a,
            .lhs = &b,
            .op = op,
        },
    };

    const result = binop.evaluate(&labels, 0);
    const expected = switch (op) {
        .Add => usize(value1) + value2,
        .Sub => usize(value1) - value2,
        .Mul => usize(value1) * value2,
        .Div => usize(value1) / value2,
        .ShiftLeft => usize(value1) << value2,
        .ShiftRight => usize(value1) >> value2,
        .AND => usize(value1) & value2,
        .OR => usize(value1) | value2,
        .XOR => usize(value1) ^ value2,
    };
    std.debug.warn("{} == {} ", result, expected);
    assert(result == expected);
}

test "Expression.evaluate Binop.Add" {
    test_binop(.Add);
}

test "Expression.evaluate Binop.Sub" {
    test_binop(.Sub);
}

test "Expression.evaluate Binop.Mul" {
    test_binop(.Mul);
}

test "Expression.evaluate Binop.Div" {
    test_binop(.Div);
}

test "Expression.evaluate Binop.ShiftLeft" {
    test_binop(.ShiftLeft);
}

test "Expression.evaluate Binop.ShiftRight" {
    test_binop(.ShiftRight);
}

test "Expression.evaluate Binop.AND" {
    test_binop(.AND);
}

test "Expression.evaluate Binop.OR" {
    test_binop(.OR);
}

test "Expression.evaluate Binop.XOR" {
    test_binop(.XOR);
}

test "Expression.evaluate CurrentLoc" {
    const labels = LabelMap.init(std.debug.global_allocator);
    const currentloc = Expression{
        .CurrentLoc = SourceLoc.init("test"),
    };

    var result = currentloc.evaluate(&labels, 0);
    std.debug.warn("{} == {} ", result, usize(0));
    assert(result == usize(0));

    result = currentloc.evaluate(&labels, 0x123);
    std.debug.warn("{} == {} ", result, usize(0x123));
    assert(result == usize(0x123));
}
