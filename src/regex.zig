const std = @import("std");
pub const Parser = @import("parser.zig").Parser;
pub const ParseTree = @import("parser.zig").ParseTree;

pub const Token = struct {
    pub const Kind = enum { Literal, LParen, RParen, Class, Dot, Star, Plus, Question, Or };
    pub const Value = union(Token.Kind) { Literal: u8, LParen, RParen, Class: RangeList, Dot, Star, Plus, Question, Or };

    kind: Token.Kind,
    value: ?Token.Value,
    pos: usize,

    pub fn format(self: *const Token, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (fmt.len != 0) { // must be {} or {any}
            return std.invalidFmtError(fmt, self);
        }

        try writer.print("{{ kind: {s}", .{@tagName(self.kind)});

        if (self.value) |val| {
            switch (val) {
                .Literal => |literal| try writer.print(", value: {c}", .{literal}),
                .Class => |rl| try writer.print(", value: {}", .{rl}),
                else => {},
            }
        }

        try writer.print(" }}", .{});
    }

    pub fn deinit(self: *const Token) void {
        if (self.value) |val| {
            switch (val) {
                .Class => |*rl| rl.deinit(),
                else => {},
            }
        }
    }
};

pub const Lexer = struct {
    start: usize,
    cursor: usize,
    regexp: []const u8,
    allocator: *std.mem.Allocator,
    tokens: std.SinglyLinkedList(Token),

    pub const Error = error{ EndOfBuffer, InvalidCharacter, UnexpectedCharater };

    pub fn init(regexp: []const u8, allocator: *std.mem.Allocator) Lexer {
        return .{ .cursor = 0, .start = 0, .regexp = regexp, .allocator = allocator, .tokens = std.SinglyLinkedList(Token){ .first = null } };
    }

    pub fn deinit(self: *Lexer) void {
        var it = self.tokens.first;

        while (it) |node| {
            if (node.data.value) |val| {
                switch (val) {
                    .Class => |*rl| rl.deinit(),
                    else => {},
                }
            }
            it = node.next;
            self.allocator.destroy(node);
        }
    }

    fn readChar(self: *Lexer) !u8 {
        if (self.cursor >= self.regexp.len) {
            return Error.EndOfBuffer;
        }

        return self.regexp[self.cursor];
    }

    pub fn scan(self: *Lexer) !std.SinglyLinkedList(Token) {
        if (self.tokens.first) |_| {
            return self.tokens;
        } else {
            self.tokens.first = try self.allocator.create(std.SinglyLinkedList(Token).Node);
        }

        var tail: ?*std.SinglyLinkedList(Token).Node = null;
        var node = self.tokens.first;

        while (true) {
            const token: Token = self.advance() catch |err| switch (err) {
                Error.EndOfBuffer => {
                    self.allocator.destroy(node.?);
                    if (tail) |t| {
                        t.next = null;
                    }
                    return self.tokens;
                },
                else => {
                    return err;
                },
            };

            node.?.data = token;
            node.?.next = try self.allocator.create(std.SinglyLinkedList(Token).Node);
            tail = node;
            node = node.?.next;
        }
    }

    pub fn advance(self: *Lexer) !Token {
        self.start = self.cursor;

        var c = try self.readChar();

        const inferred_type: Token.Kind = switch (c) {
            '(' => .LParen,
            ')' => .RParen,
            '[' => .Class,
            '.' => .Dot,
            '*' => .Star,
            '+' => .Plus,
            '?' => .Question,
            '|' => .Or,
            else => .Literal,
        };

        switch (inferred_type) {
            .Literal => {
                if (c == '\\') {
                    self.cursor += 1;
                    c = try self.readChar(); // needs more verbose error handling
                    c = escapedChar(c);
                }
                self.cursor += 1;
                return .{ .kind = inferred_type, .value = .{ .Literal = c }, .pos = self.start };
            },
            .Class => {
                while (c != ']') { // needs more verbose error handling
                    c = try self.readChar();
                    self.cursor += 1;
                }
                return .{ .kind = inferred_type, .value = .{ .Class = try RangeList.init(self.regexp[self.start + 1 .. self.cursor - 1], self.allocator) }, .pos = self.start };
            },
            else => {
                self.cursor += 1;
                return .{ .kind = inferred_type, .pos = self.start, .value = null };
            },
        }
    }

    fn escapedChar(c: u8) u8 {
        return switch (c) {
            'n' => '\n', // placeholder for now
            't' => '\t',
            'r' => '\r',
            else => c,
        };
    }
};

// could use something like a bst, but thinking about what regex classes people actually write makes me think that a list is more efficient
pub const RangeList = struct {
    negated: bool,
    head: ?*Node,
    allocator: *std.mem.Allocator,

    const Node = struct {
        range: struct { min: u8, max: u8 },
        next: ?*Node,
    };

    // need to do more error handling
    pub fn init(str: []const u8, allocator: *std.mem.Allocator) !RangeList {
        var rl = RangeList{
            .negated = false,
            .head = null,
            .allocator = allocator,
        };

        var i: usize = 0;

        if (str.len == 0) {
            return rl;
        }

        if (str[0] == '^') {
            rl.negated = true;
            i += 1;
        }

        var p = rl.head;
        while (i < str.len) : (i += 1) {
            const node = try rl.allocator.create(Node);

            const min = str[i];
            var max = min;

            i += 1;

            if (i + 1 < str.len and str[i] == '-') {
                i += 1;
                max = str[i];
                i += 1;
            }

            node.* = .{
                .range = .{ .min = min, .max = max },
                .next = null,
            };

            if (p) |tail| {
                tail.next = node;
                p = tail.next;
            } else {
                rl.head = node;
                p = rl.head;
            }
        }
        return rl;
    }

    pub fn deinit(self: *const RangeList) void {
        var itr = self.head;

        while (itr) |node| {
            const prev = node;
            itr = node.next;
            self.allocator.destroy(prev);
        }
    }

    pub fn format(self: *const RangeList, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        var itr = self.head;

        if (fmt.len != 0) { // must be {} or {any}
            return std.invalidFmtError(fmt, self);
        }

        if (self.negated) {
            try writer.print("^", .{});
        }

        try writer.print("[", .{});

        while (itr) |node| : (itr = node.next) {
            if (node.range.min == node.range.max) {
                try writer.print("{c}", .{node.range.min});
            } else {
                try writer.print("{c}-{c}", .{ node.range.min, node.range.max });
            }

            if (node.next) |_| {
                try writer.print(", ", .{});
            }
        }
        try writer.print("]", .{});
    }
};
