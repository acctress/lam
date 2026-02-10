const std = @import("std");
const builtin = @import("builtin");
const lam = @import("lam");
const ArrayList = std.ArrayList;

const TokenType = enum(u8) {
    LParen,
    RParen,
    Number,
    Symbol,
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

const ValueType = enum { integer, list };

const Value = union(ValueType) {
    integer: i64,
    list: []i64,

    pub fn fmt(self: Value, comptime f: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = f;
        _ = options;

        switch (self) {
            .integer => |i| try writer.print("{d}", .{i}),
            .list => |l| {
                try writer.print("[", .{});
                for (l, 0..) |i, idx| {
                    try writer.print("{d}", .{i});
                    if (idx < l.len - 1) try writer.print(", ", .{});
                }
                try writer.print("]\n", .{});
            },
        }
    }
};

const NodeType = enum(u8) { partial, application, literal };

const Node = union(NodeType) {
    partial: Partial,
    application: Application,
    literal: i64,

    const Partial = struct {
        op: []const u8,
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
            'a'...'z', 'A'...'Z' => b: {
                const start = pos;
                len = 1;
                while (start + len < source.len and std.ascii.isAlphabetic(source[start + len])) {
                    len += 1;
                }

                break :b .{ .type = .Symbol, .value = source[start .. start + len] };
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
            .partial => |s| {
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

    pub fn parse(self: *Parser) !?*Node {
        if (self.check(.EOF)) return null;

        const exp = try self.parse_expr();
        try self.ast.append(self.allocator, exp);

        return exp;
    }

    fn parse_expr(self: *Parser) !*Node {
        // partial, literal...
        const left = try self.parse_primary();

        // if it's followed by an expression its an application
        while (self.check(.Number) or self.check(.LParen)) {
            const right = try self.parse_primary();
            return try self.alloc_node(.{ .application = .{ .func = left, .arg = right } });
        }

        return left;
    }

    fn parse_primary(self: *Parser) !*Node {
        if (self.check(.LParen)) {
            return try self.alloc_node(.{ .partial = try self.parse_partial() });
        } else if (self.check(.Number)) {
            return try self.alloc_node(.{ .literal = try std.fmt.parseInt(i64, (try self.expect(.Number)).value, 10) });
        }

        return ParserError.UnexpectedToken;
    }

    fn parse_partial(self: *Parser) error{ UnexpectedToken, OutOfMemory, InvalidCharacter, Overflow }!Node.Partial {
        _ = try self.expect(.LParen);

        const op = self.current.?;
        if (op.type != .Symbol and
            op.type != .Add and
            op.type != .Sub and
            op.type != .Mul and
            op.type != .Div) return error.UnexpectedToken;

        const op_val = op.value;
        _ = self.eat();

        const arg = try self.parse_expr();

        _ = try self.expect(.RParen);

        return .{ .op = op_val, .arg = arg };
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

const EvalError = error{ CannotEvalPartial, Unreachable, TypeError };

fn eval(allocator: std.mem.Allocator, node: *Node) !Value {
    switch (node.*) {
        .literal => |i| return Value{ .integer = i },
        .partial => return EvalError.CannotEvalPartial,
        .application => |a| {
            const fun = a.func.*;
            const aval = try eval(allocator, a.arg);

            if (aval != .integer) return EvalError.TypeError;
            const val = aval.integer;

            if (fun == .partial) {
                const sect_arg = try eval(allocator, fun.partial.arg);
                if (sect_arg != .integer) return EvalError.TypeError;
                const arg = sect_arg.integer;
                const op = fun.partial.op;

                // list
                if (std.mem.eql(u8, op, "list")) {
                    const arr = try allocator.alloc(i64, @intCast(val));
                    @memset(arr, arg);
                    return Value{ .list = arr };
                }

                const lhv = sect_arg.integer;
                const rhv = aval.integer;

                return Value{ .integer = switch (op[0]) {
                    '+' => lhv + rhv,
                    '-' => rhv - lhv,
                    '*' => lhv * rhv,
                    '/' => @divTrunc(lhv, rhv),
                    else => 0,
                } };
            } else {
                return EvalError.Unreachable;
            }
        },
    }
}

pub fn main() !void {
    if (builtin.os.tag == .windows) {
        const win = std.os.windows;
        _ = win.kernel32.SetConsoleOutputCP(65001);
    }

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var stdout_buf: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buf);
    const stdout: *std.io.Writer = &stdout_writer.interface;

    var stdin_buf: [1024]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buf);
    const stdin: *std.io.Reader = &stdin_reader.interface;

    repl: while (true) {
        try stdout.print("{s} ", .{"\u{03bb}"});
        try stdout.flush();

        const raw_line = try stdin.takeDelimiter('\n') orelse unreachable;
        const line = std.mem.trim(u8, raw_line, "\r");

        if (std.mem.eql(u8, line, "quit")) break :repl;

        var parser: Parser = try .init(allocator, line);
        defer parser.deinit();

        while (try parser.parse()) |n| {
            const result = eval(allocator, n) catch |err| {
                try stdout.print("error: {}\n", .{err});
                break :repl;
            };

            try result.fmt("{}\n", .{}, stdout);

            // try stdout.print("{}\n", .{result.fmt(comptime f: []const u8, options: Options, writer: anytype)});
            try stdout.flush();
        }
    }
}
