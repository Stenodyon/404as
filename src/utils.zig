const std = @import("std");

pub fn streqi(comptime base: []const u8, b: []const u8) bool {
    if (base.len != b.len) return false;
    var i: usize = 0;
    while (i < base.len) : (i += 1) {
        if (base[i] != std.ascii.toUpper(b[i])) {
            return false;
        }
    }
    return true;
}

pub const SourceLoc = struct {
    filename: []const u8,
    row: usize,
    col: usize,

    pub fn init(filename: []const u8) SourceLoc {
        return SourceLoc{
            .filename = filename,
            .row = 1,
            .col = 0,
        };
    }
};

pub fn warn(loc: SourceLoc, comptime fmt: []const u8, args: ...) void {
    std.debug.warn("{}:{}:{} warning: ", loc.filename, loc.row, loc.col);
    std.debug.warn(fmt, args);
}

pub fn fail(loc: SourceLoc, comptime fmt: []const u8, args: ...) noreturn {
    std.debug.warn("{}:{}:{} error: ", loc.filename, loc.row, loc.col);
    std.debug.warn(fmt, args);
    std.process.exit(1);
}
