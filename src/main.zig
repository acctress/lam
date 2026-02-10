const std = @import("std");
const lam = @import("lam");
const ArrayList = std.ArrayList;

const TokenType = enum(u8) {
    LParen,
    RParen,
    Number,
    Add,
    Sub,
    Mul,
    Div,
    EOF,
};

const Token = struct {
    type: TokenType,
    value: []const u8,
};

const NodeType = enum(u8) { section, application, literal };

const Node = union(NodeType) {
    section: Section,
    application: Application,
    literal: i64,

    const Section = struct {
        op: u8,
        arg: *Node,
    };

    const Application = struct { func: *Node, arg: *Node };
};

fn tokenize(allocator: std.mem.Allocator, source: []const u8) !ArrayList(Token) {
    var tokens: ArrayList(Token) = .empty;
    var pos: usize = 0;

    while (pos < source.len) {
        const cur = source[pos];
        var len: usize = 1;

        if (std.ascii.isWhitespace(cur)) {
            pos += 1;
            continue;
        }

        const token: Token = switch (cur) {
            '(' => .{ .type = .LParen, .value = "(" },
            ')' => .{ .type = .RParen, .value = ")" },
            '+' => .{ .type = .Add, .value = "+" },
            '-' => .{ .type = .Sub, .value = "-" },
            '*' => .{ .type = .Mul, .value = "*" },
            '/' => .{ .type = .Div, .value = "/" },
            '0'...'9' => b: {
                const start = pos;
                len = 1;
                while (start + len < source.len and std.ascii.isDigit(source[start + len])) {
                    len += 1;
                }

                break :b .{ .type = .Number, .value = source[start .. start + len] };
            },
            else => .{ .type = .EOF, .value = "" },
        };

        pos += len;
        try tokens.append(allocator, token);
    }

    try tokens.append(allocator, .{ .type = .EOF, .value = "" });

    return tokens;
}

const ParserError = error{UnexpectedToken};

const Parser = struct {
    allocator: std.mem.Allocator,
    ast: ArrayList(*Node),
    tokens: ArrayList(Token),
    current: ?Token,
    pos: usize,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Parser {
        const tokens = try tokenize(allocator, source);
        return .{
            .allocator = allocator,
            .ast = .empty,
            .tokens = tokens,
            .current = tokens.items[0],
            .pos = 0,
        };
    }

    pub fn deinit(self: *Parser) void {
        for (self.ast.items) |node| {
            self.free_node(node);
        }

        self.tokens.deinit(self.allocator);
        self.ast.deinit(self.allocator);
    }

    fn free_node(self: *Parser, node: *Node) void {
        switch (node.*) {
            .section => |s| {
                self.free_node(s.arg);
                // self.allocator.destroy(s.arg);
            },
            .application => |a| {
                self.free_node(a.func);
                self.free_node(a.arg);
            },
            .literal => {},
        }

        self.allocator.destroy(node);
    }

    fn alloc_node(self: *Parser, node: Node) !*Node {
        const p = try self.allocator.create(Node);
        p.* = node;
        return p;
    }

    pub fn parse(self: *Parser) !ArrayList(*Node) {
        while (!self.check(.EOF)) {
            const exp = try self.parse_expr();
            try self.ast.append(self.allocator, exp);
        }

        return self.ast;
    }

    fn parse_expr(self: *Parser) !*Node {
        // section, literal...
        const left = try self.parse_primary();

        // if it's followed by an expression its an application
        if (self.check(.Number) or self.check(.LParen)) {
            const right = try self.parse_primary();
            return try self.alloc_node(.{ .application = .{ .func = left, .arg = right } });
        }

        return left;
    }

    fn parse_primary(self: *Parser) !*Node {
        if (self.check(.LParen)) {
            return try self.alloc_node(.{ .section = try self.parse_section() });
        } else if (self.check(.Number)) {
            return try self.alloc_node(.{ .literal = try std.fmt.parseInt(i64, (try self.expect(.Number)).value, 10) });
        }

        return ParserError.UnexpectedToken;
    }

    fn parse_section(self: *Parser) error{ UnexpectedToken, OutOfMemory, InvalidCharacter, Overflow }!Node.Section {
        _ = try self.expect(.LParen);

        const op: u8 = self.current.?.value[0];
        _ = self.eat();

        const arg = try self.parse_expr();

        _ = try self.expect(.RParen);

        return .{ .op = op, .arg = arg };
    }

    fn eat(self: *Parser) Token {
        if (self.check(.EOF)) {
            std.debug.print("unexpected eof", .{});
            std.process.exit(1);
        }

        const prev = self.current.?;
        self.pos += 1;
        self.current = self.tokens.items[self.pos];
        return prev;
    }

    fn check(self: *Parser, typ: TokenType) bool {
        return self.current.?.type == typ;
    }

    fn expect(self: *Parser, typ: TokenType) ParserError!Token {
        if (!self.check(typ)) {
            std.debug.print("expected {}, got {}\n", .{ typ, self.current.?.type });
            return ParserError.UnexpectedToken;
        }

        return self.eat();
    }
};

const EvalError = error{ CannotEvalSection, Unreachable };

fn eval(node: *Node) EvalError!i64 {
    switch (node.*) {
        .literal => |i| return i,
        .section => return EvalError.CannotEvalSection,
        .application => |a| {
            const fun = a.func.*;
            const aval = try eval(a.arg);
            if (fun == .section) {
                const sect_arg = try eval(fun.section.arg);
                const op = fun.section.op;

                return switch (op) {
                    '+' => sect_arg + aval,
                    '-' => sect_arg - aval,
                    '*' => sect_arg * aval,
                    '/' => @divTrunc(sect_arg, aval),
                    else => EvalError.Unreachable,
                };
            } else {
                return EvalError.Unreachable;
            }
        },
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();
    const source = "(+ 1) 5";

    var parser: Parser = try .init(allocator, source);
    defer parser.deinit();

    std.debug.print("source: {s}\n", .{source});

    const ast = try parser.parse();
    for (ast.items) |node| {
        switch (node.*) {
            .section => {
                std.debug.print("section:\n", .{});
                std.debug.print("   op: {c}\n", .{node.section.op});
                std.debug.print("   arg: ", .{});

                switch (node.section.arg.*) {
                    .literal => std.debug.print("{d}\n", .{node.section.arg.*.literal}),
                    else => std.debug.print("?\n", .{}),
                }
            },
            .application => {
                std.debug.print("application:\n", .{});
                std.debug.print("   fn: {*}\n", .{&node.application.func});
                std.debug.print("   arg: ", .{});

                switch (node.application.arg.*) {
                    .literal => std.debug.print("{d}\n", .{node.application.arg.*.literal}),
                    else => std.debug.print("?\n", .{}),
                }
            },
            else => break,
        }
    }

    const result = try eval(ast.items[0]);
    std.debug.print("eval: {}", .{result});
}
