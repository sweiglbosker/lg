const std = @import("std");
const Regex = @import("regex.zig");

pub fn main() !void {
    const stdio = std.io.getStdIn().writer();
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();

    defer _ = gpa.deinit();

    var lexer = Regex.Lexer.init("bz*[a-z](l)", &allocator);

    _ = try lexer.scan();

    var parser = Regex.Parser.init(&lexer.tokens, &allocator);

    const pt = try parser.parse();

    try stdio.print("{}\n", .{pt});

    parser.deinit();
    lexer.deinit();
}
