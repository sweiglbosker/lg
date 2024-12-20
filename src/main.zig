const std = @import("std");
const Regex = @import("regex.zig");

pub fn main() !void {
    const stdio = std.io.getStdIn().writer();
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();

    defer _ = gpa.deinit();

    const lexer = try allocator.create(Regex.Lexer);

    lexer.* = Regex.Lexer.init("abc|123[a-Z]()+", &allocator);

    const tl = try lexer.scan();

    var it = tl.first;
    while (it) |node| : (it = node.next) {
        try stdio.print("{}\n", .{&(node.data)});
    }

    const parser = try allocator.create(Regex.Parser);

    parser.* = Regex.Parser.init(&lexer.tokens, &allocator);

    _ = try parser.parseRe();

    lexer.deinit();
    allocator.destroy(lexer);
    allocator.destroy(parser);
}
