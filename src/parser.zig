const std = @import("std");
const Token = @import("regex.zig").Token;

const TokenList = std.SinglyLinkedList(Token);

pub const Rule = enum { Terminal, Re, Rer, Cat, Catr, E, L };

pub const ParseTree = struct {
    rule: Rule,
    token: ?Token,
    sibling: ?*ParseTree,
    child: ?*ParseTree,

    pub fn init(rule: Rule, allocator: *std.mem.Allocator) !*ParseTree {
        const t = try allocator.create(ParseTree);

        t.* = .{
            .rule = rule,
            .token = null,
            .sibling = null,
            .child = null,
        };

        return t;
    }

    pub fn deinit(self: *ParseTree, allocator: *std.mem.Allocator) void {
        const child = self.child;
        const sibling = self.sibling;

        allocator.destroy(self);

        if (child) |c| {
            c.deinit(allocator);
        }

        if (sibling) |s| {
            s.deinit(allocator);
        }
    }

    pub fn appendChild(self: *ParseTree, child: *ParseTree) void {
        if (self.child) |c| {
            var p = c;
            while (p.sibling) |sibling| {
                p = sibling;
            }
            p.sibling = child;
        } else {
            self.child = child;
        }
    }

    pub fn format(self: *const ParseTree, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();
        const allocator = arena.allocator();

        if (fmt.len != 0) {
            return std.invalidFmtError(fmt, self);
        }

        const Queue = std.DoublyLinkedList(struct { *const ParseTree, u32 });

        var q: Queue = .{};

        const root = try allocator.create(Queue.Node);
        root.* = .{ .data = .{ self, 0 } };

        q.append(root);

        var level: u32 = 0;
        while (q.len != 0) {
            const cur: *Queue.Node = q.popFirst().?;

            const tree = cur.data[0];
            const l = cur.data[1];

            if (l != level) {
                try writer.print("\n", .{});
                level = l;
            }

            try writer.print("{s} ", .{@tagName(tree.rule)});

            var p: ?*ParseTree = tree.child;
            while (p) |child| : (p = child.sibling) {
                const node = try allocator.create(Queue.Node);
                node.* = .{ .data = .{ child, l + 1 } };
                q.append(node);
            }
        }
    }
};

pub const Parser = struct {
    parse_tree: ?*ParseTree,
    tokens: *const TokenList,
    itr: ?*TokenList.Node,
    allocator: *std.mem.Allocator,

    pub const Error = error{ UnexpectedToken, ExpectedToken };

    pub fn init(tl: *const TokenList, allocator: *std.mem.Allocator) Parser {
        return .{ .tokens = tl, .allocator = allocator, .itr = tl.first.?, .parse_tree = null };
    }

    pub fn deinit(self: *Parser) void {
        if (self.parse_tree) |pt| {
            pt.deinit(self.allocator);
        }
    }

    pub fn expectToken(self: *Parser, kind: Token.Kind) !*ParseTree {
        if (self.itr) |node| {
            if (node.data.kind == kind) {
                const t = try ParseTree.init(Rule.Terminal, self.allocator);

                t.token = node.data;

                self.itr = node.next;

                return t;
            } else {
                std.debug.print("{d} | error: expected token of kind {s} but found token of kind {s}", .{ node.data.pos, @tagName(kind), @tagName(node.data.kind) });
                return Error.UnexpectedToken;
            }
        } else {
            std.debug.print("error: expected token of kind {s} but reached end of stream!\n", .{@tagName(kind)}); // should use actual logging eventually
            return Error.ExpectedToken;
        }
    }

    pub fn parse(self: *Parser) !*ParseTree {
        if (self.parse_tree) |pt| {
            return pt;
        }

        self.parse_tree = try self.parseRe();

        return self.parse_tree.?;
    }

    fn parseRe(self: *Parser) error{ ExpectedToken, UnexpectedToken, OutOfMemory }!*ParseTree {
        const t = try ParseTree.init(Rule.Re, self.allocator);
        errdefer t.deinit(self.allocator);

        t.appendChild(try self.parseCat());
        t.appendChild(try self.parseRer());

        return t;
    }

    fn parseRer(self: *Parser) !*ParseTree {
        const t = try ParseTree.init(Rule.Rer, self.allocator);
        errdefer t.deinit(self.allocator);

        if (self.itr) |node| {
            if (node.data.kind == Token.Kind.Or) {
                t.appendChild(try self.expectToken(Token.Kind.Or));
                t.appendChild(try self.parseCat());
            }
        } // else -> epsilon

        return t;
    }

    fn parseCat(self: *Parser) !*ParseTree {
        const t = try ParseTree.init(Rule.Cat, self.allocator);
        errdefer t.deinit(self.allocator);

        t.appendChild(try self.parseE());
        t.appendChild(try self.parseCatr());

        return t;
    }

    fn parseCatr(self: *Parser) !*ParseTree {
        const t = try ParseTree.init(Rule.Catr, self.allocator);
        errdefer t.deinit(self.allocator);

        if (self.itr) |node| {
            switch (node.data.kind) {
                .RParen, .Or => {}, // also cases where the null production should be chosen
                else => {
                    t.appendChild(try self.parseE());
                    t.appendChild(try self.parseCatr());
                },
            }
        }

        return t;
    }

    fn parseE(self: *Parser) !*ParseTree {
        const t = try ParseTree.init(Rule.E, self.allocator);
        errdefer t.deinit(self.allocator);

        t.appendChild(try self.parseL());

        if (self.itr) |node| {
            switch (node.data.kind) {
                .Plus, .Question, .Star => {
                    t.appendChild(try self.expectToken(node.data.kind)); // bruh
                },
                else => {},
            }
        }

        return t;
    }

    fn parseL(self: *Parser) !*ParseTree {
        const t = try ParseTree.init(Rule.L, self.allocator);
        errdefer t.deinit(self.allocator);

        if (self.itr) |node| {
            switch (node.data.kind) {
                .Literal, .Class, .Dot => {
                    t.appendChild(try self.expectToken(node.data.kind)); // bruh
                },
                .LParen => {
                    t.appendChild(try self.expectToken(Token.Kind.LParen));
                    t.appendChild(try self.parseRe());
                    t.appendChild(try self.expectToken(Token.Kind.RParen));
                },
                else => {
                    std.debug.print("unexpected token of type {s}\n", .{@tagName(node.data.kind)});
                    return Error.UnexpectedToken;
                },
            }
        } else {
            return Error.ExpectedToken;
        }

        return t;
    }
};
