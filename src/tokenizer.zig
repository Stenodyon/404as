const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Utf8Iterator = std.unicode.Utf8Iterator;

usingnamespace @import("instructions.zig");

pub const SourceLoc = struct {
    filename: []const u8,
    row: usize,
    col: usize,

    pub fn init(filename: []const u8) SourceLoc {
        return SourceLoc{
            .filename = filename,
            .row = 0,
            .col = 0,
        };
    }
};

pub const TokenId = union(enum) {
    Comma,
    Colon,
    Symbol,
    Comment,
    Newline,
};

pub const Token = struct {
    id: TokenId,
    loc: SourceLoc,
    contents: []const u8,

    pub fn debug_print(token: *Token) void {
        std.debug.warn("{}(\"{}\")", @tagName(token.id), token.contents);
    }
};

const TokenizeState = enum {
    Start,
    Symbol,
    Comment,
};

const Tokenize = struct {
    source: []const u8,
    iter: Utf8Iterator,
    state: TokenizeState,
    loc: SourceLoc,
    tokens: ArrayList(Token),
    prevPos: usize,
    beginPos: usize,

    pub fn init(
        allocator: *Allocator,
        filename: []const u8,
        source: []const u8,
    ) Tokenize {
        return Tokenize{
            .source = source,
            .iter = Utf8Iterator{ .bytes = source, .i = 0 },
            .state = .Start,
            .loc = SourceLoc.init(filename),
            .tokens = ArrayList(Token).init(allocator),
            .prevPos = 0,
            .beginPos = 0,
        };
    }

    pub fn next_char(self: *Tokenize) ?u32 {
        self.prevPos = self.iter.i;
        self.loc.col += 1;
        return self.iter.nextCodepoint();
    }

    pub fn revert_char(self: *Tokenize) void {
        self.iter.i = self.prevPos;
    }

    pub fn begin_token(self: *Tokenize, id: TokenId) !void {
        try self.tokens.append(Token{
            .id = id,
            .loc = self.loc,
            .contents = [_]u8{},
        });
        self.beginPos = self.prevPos;
    }

    pub fn end_token(self: *Tokenize) void {
        var token = &self.tokens.items[self.tokens.len - 1];
        token.contents = self.source[self.beginPos..self.iter.i];
    }

    pub fn newline(self: *Tokenize) void {
        self.loc.col = 0;
        self.loc.row += 1;
    }

    pub fn fail(self: *Tokenize, comptime fmt: []const u8, args: ...) void {
        std.debug.warn(
            "{}:{}:{}: ",
            self.loc.filename,
            self.loc.row,
            self.loc.col,
        );
        std.debug.warn(fmt, args);
        std.process.exit(1);
    }
};

pub fn tokenize(
    allocator: *Allocator,
    filename: []const u8,
    source: []const u8,
) ![]Token {
    var t = Tokenize.init(allocator, filename, source);

    while (t.next_char()) |c| {
        switch (t.state) {
            .Start => {
                switch (c) {
                    ' ', '\t', '\r' => {},
                    '\n' => {
                        t.newline();
                        try t.begin_token(.Newline);
                        t.end_token();
                    },
                    'a'...'z', 'A'...'Z', '_' => {
                        try t.begin_token(.Symbol);
                        t.state = .Symbol;
                    },
                    ';' => {
                        try t.begin_token(.Comment);
                        t.state = .Comment;
                    },
                    ':' => {
                        try t.begin_token(.Comma);
                        t.end_token();
                    },
                    ',' => {
                        try t.begin_token(.Colon);
                        t.end_token();
                    },
                    else => {
                        var buffer: [@sizeOf(u32)]u8 = undefined;
                        const size = try std.unicode.utf8Encode(c, buffer[0..]);
                        t.fail("Invalid character '{}'\n", buffer[0..size]);
                    },
                }
            },
            .Symbol => {
                switch (c) {
                    'a'...'z', 'A'...'Z', '0'...'9', '_' => {},
                    else => {
                        t.revert_char();
                        t.end_token();
                        t.state = .Start;
                    },
                }
            },
            .Comment => {
                switch (c) {
                    '\n' => {
                        t.revert_char();
                        t.end_token();
                        t.state = .Start;
                    },
                    else => {},
                }
            },
        }
    }

    return t.tokens.toOwnedSlice();
}
