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
};

pub const Parser = struct {
    tokens: *const TokenList,
    itr: ?*TokenList.Node,
    allocator: *std.mem.Allocator,

    pub const Error = error{ UnexpectedToken, ExpectedToken };

    pub fn init(tl: *const TokenList, allocator: *std.mem.Allocator) Parser {
        return .{ .tokens = tl, .allocator = allocator, .itr = tl.first.? };
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

    pub fn parseRe(self: *Parser) error{ ExpectedToken, UnexpectedToken, OutOfMemory }!*ParseTree {
        const t = try ParseTree.init(Rule.Re, self.allocator);

        t.appendChild(try self.parseCat());
        t.appendChild(try self.parseRer());

        return t;
    }

    pub fn parseRer(self: *Parser) !*ParseTree {
        const t = try ParseTree.init(Rule.Rer, self.allocator);

        if (self.itr) |_| {
            t.appendChild(try self.expectToken(Token.Kind.Or));
            t.appendChild(try self.parseCat());
        } // else -> epsilon

        return t;
    }

    pub fn parseCat(self: *Parser) !*ParseTree {
        const t = try ParseTree.init(Rule.Cat, self.allocator);

        t.appendChild(try self.parseE());
        t.appendChild(try self.parseCatr());

        return t;
    }

    pub fn parseCatr(self: *Parser) !*ParseTree {
        const t = try ParseTree.init(Rule.Catr, self.allocator);

        if (self.itr) |_| {
            t.appendChild(try self.parseE());
            t.appendChild(try self.parseCatr());
        }

        return t;
    }

    pub fn parseE(self: *Parser) !*ParseTree {
        const t = try ParseTree.init(Rule.E, self.allocator);

        t.appendChild(try self.parseL());

        if (self.itr) |node| {
            switch (node.data.kind) {
                .Plus, .Question, .Star => {
                    t.appendChild(try self.expectToken(node.data.kind)); // bruh
                },
                else => {},
            }
        } else {
            return Error.ExpectedToken;
        }

        return t;
    }

    pub fn parseL(self: *Parser) !*ParseTree {
        const t = try ParseTree.init(Rule.L, self.allocator);

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
                    return Error.UnexpectedToken;
                },
            }
        } else {
            return Error.ExpectedToken;
        }

        return t;
    }
};
