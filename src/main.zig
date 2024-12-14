const std = @import("std");
const Regex = @import("regex.zig");

pub fn main() !void {
    const stdio = std.io.getStdIn().writer();
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();

    defer _ = gpa.deinit();

    const lexer = try allocator.create(Regex.Lexer);
    lexer.* = Regex.Lexer.init("abc|123[a-Z]()+", &allocator);
    defer allocator.destroy(lexer);

    while (true) {
        const token = lexer.advance() catch |err| switch (err) {
            Regex.Lexer.Error.EndOfBuffer => {
                break;
            },
            else => {
                return err;
            },
        };

        try stdio.print("{}\n", .{token});

        if (token.value) |v| {
            switch (v) {
                .Class => |*rl| rl.deinit(),
                else => {},
            }
        }
    }
}
