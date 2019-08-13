const std = @import("std");

const tokenizer = @import("tokenizer.zig");
const parser = @import("parser.zig");
const codegen = @import("gen.zig");

pub fn main() anyerror!void {
    const args = try std.process.argsAlloc(std.heap.c_allocator);
    defer std.process.argsFree(std.heap.c_allocator, args);
    if (args.len != 2) {
        std.debug.warn("Usage: {} asm-file\n", args[0]);
        std.process.exit(1);
    }
    const filename = args[1];
    const contents = try std.io.readFileAlloc(std.heap.c_allocator, filename);
    defer std.heap.c_allocator.free(contents);

    //const tokens = try tokenizer.tokenize(std.heap.c_allocator, filename, contents);
    //defer std.heap.c_allocator.free(tokens);

    //std.debug.warn("{} tokens tokenized\n", tokens.len);
    //for (tokens) |*token| {
    //    token.debug_print();
    //    std.debug.warn(" ");
    //}

    //const program = parser.parse(std.heap.c_allocator, filename, contents);
    const assembly = try codegen.assemble(
        std.heap.c_allocator,
        filename,
        contents,
    );
    defer std.heap.c_allocator.free(assembly);

    try std.io.writeFile("a.out", assembly);
}
