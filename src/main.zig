const std = @import("std");
const builtin = @import("builtin");
const ArrayList = std.ArrayList;

const TokenType = enum(u8) {
    LParen,
    RParen,
    Number,
    String,
    Char,
    Symbol,
    LBracket,
    RBracket,
    DoubleDot,
    Comma,
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

const ValueType = enum { integer, list, string, char, function, unit }; // unit is void but void is a keyword already so cant us it

const Value = union(ValueType) {
    integer: i64,
    list: []i64,
    string: []const u8,
    char: u8,
    function: *Node,
    unit: void,

    pub fn fmt(self: Value, comptime f: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = f;
        _ = options;

        try writer.print("\x1b[38;5;210m{s}\x1b[0m \x1b[38;5;146m", .{"\u{02192}"});

        switch (self) {
            .unit => {},
            .function => |_| try writer.print("@function\x1b[0m\n", .{}),
            .integer => |i| try writer.print("{d}\x1b[0m\n", .{i}),
            .string => |s| try writer.print("{s}\x1b[0m\n", .{s}),
            .char => |c| try writer.print("{c}\x1b[0m\n", .{c}),
            .list => |l| {
                try writer.print("[", .{});
                for (l, 0..) |i, idx| {
                    try writer.print("{d}", .{i});
                    if (idx < l.len - 1) try writer.print(", ", .{});
                }
                try writer.print("]\x1b[0m\n", .{});
            },
        }
    }
};

const IntrinsicFN = *const fn (allocator: std.mem.Allocator, args: []const Value) anyerror!Value;

const Intrinsic = struct { name: []const u8, func: IntrinsicFN };

const INTRINSICS = [_]Intrinsic{
    .{ .name = "map", .func = intrinsic_map },
    .{ .name = "compose", .func = intrinsic_compose },
    .{ .name = "list", .func = intrinsic_list },
    .{ .name = "get", .func = intrinsic_get },
    .{ .name = "putln", .func = intrinsic_putln },
    .{ .name = "str", .func = intrinsic_str },
    .{ .name = "chars", .func = intrinsic_chars },
};

const NodeType = enum(u8) { partial, application, literal, list, string, char, composition, intrinsic };

const Node = union(NodeType) {
    partial: Partial,
    application: Application,
    literal: i64,
    list: []i64,
    string: []const u8,
    char: u8,
    composition: Composition,
    intrinsic: intrinsicNode,

    const Partial = struct {
        op: []const u8,
        arg: *Node,
    };

    const Application = struct { func: *Node, arg: *Node };

    const Composition = struct {
        outer: *Node,
        inner: *Node,
    };

    const intrinsicNode = struct {
        name: []const u8,
        args: []*Node,
    };
};

fn get_intrinsic(name: []const u8) ?Intrinsic {
    for (INTRINSICS) |i| {
        if (std.mem.eql(u8, i.name, name)) return i;
    }

    return null;
}

const TokenizeError = error{ UnterminatedStringLiteral, InvalidCharacterLiteral, OutOfMemory };

fn tokenize(allocator: std.mem.Allocator, source: []const u8) TokenizeError!ArrayList(Token) {
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
            '[' => .{ .type = .LBracket, .value = "[" },
            ']' => .{ .type = .RBracket, .value = "]" },
            ',' => .{ .type = .Comma, .value = "," },
            '.' => b: {
                if (pos + 1 < source.len and source[pos + 1] == '.') {
                    len = 2;
                    break :b .{ .type = .DoubleDot, .value = ".." };
                }

                continue;
            },
            '\'' => b: {
                const start = pos;
                len = 1;

                if (start + 1 >= source.len) {
                    return TokenizeError.UnterminatedStringLiteral;
                }

                if (!std.ascii.isAlphanumeric(source[start + 1])) {
                    return TokenizeError.InvalidCharacterLiteral;
                }

                len += 1;

                if (start + len >= source.len or source[start + len] != '\'') {
                    return TokenizeError.InvalidCharacterLiteral;
                }

                const slice = source[start + 1 .. start + 2];
                len += 1;
                break :b .{ .type = .Char, .value = slice };
            },
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
            '"' => b: {
                const start = pos + 1;
                len = 1;
                while (pos + len < source.len and source[pos + len] != '"') {
                    len += 1;
                }

                const val = source[start .. pos + len];
                if (pos + len >= source.len) {
                    return TokenizeError.UnterminatedStringLiteral;
                }

                len += 1;

                break :b .{ .type = .String, .value = val };
            },
            else => .{ .type = .EOF, .value = "" },
        };

        pos += len;
        try tokens.append(allocator, token);
    }

    try tokens.append(allocator, .{ .type = .EOF, .value = "" });

    return tokens;
}

const ParserError = error{ UnexpectedToken, InvalidCharacter, Overflow, OutOfMemory };

const Parser = struct {
    allocator: std.mem.Allocator,
    ast: ArrayList(*Node),
    tokens: ArrayList(Token),
    pos: usize,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Parser {
        const tokens = try tokenize(allocator, source);
        return .{
            .allocator = allocator,
            .ast = .empty,
            .tokens = tokens,
            .pos = 0,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.tokens.deinit(self.allocator);
        self.ast.deinit(self.allocator);
    }

    pub fn parse(self: *Parser) !?*Node {
        if (self.peek().type == .EOF) return null;

        const node = try self.parse_expr(0);
        try self.ast.append(self.allocator, node);
        return node;
    }

    pub fn parse_expr(self: *Parser, prec: u8) !*Node {
        var left = try self.parse_null_deno();

        while (true) {
            const next = self.peek();
            // treat the next token as an infix operator if
            // the token looks like the start of an expression
            const next_pr = self.get_precedence(next);
            if (prec >= next_pr) break;

            left = try self.parse_left_deno(left);
        }

        return left;
    }

    fn parse_null_deno(self: *Parser) ParserError!*Node {
        // null denominator
        const current = self.consume();
        return switch (current.type) {
            .Number => try self.alloc_node(.{ .literal = try std.fmt.parseInt(i64, current.value, 10) }),
            .String => try self.alloc_node(.{ .string = current.value }),
            .Char => try self.alloc_node(.{ .char = current.value[0] }),
            .Symbol => {
                if (get_intrinsic(current.value)) |_| {
                    const args = try self.allocator.alloc(*Node, 0);
                    return try self.alloc_node(.{ .intrinsic = .{ .name = current.value, .args = args } });
                }

                return error.UnexpectedToken;
            },
            .LBracket => {
                // list or range list
                if (self.peek().type == .Number) {
                    const start = self.consume();

                    if (self.peek().type == .DoubleDot) {
                        _ = self.consume();
                        // past [<num>..
                        const end = try self.expect(.Number);
                        const start_v = try std.fmt.parseInt(i64, start.value, 10);
                        const end_v = try std.fmt.parseInt(i64, end.value, 10);

                        // 1 is the default step
                        var step: i64 = 1;
                        // if there is a comma after the range, we expect for the user
                        // to define a step
                        if (self.peek().type == .Comma) {
                            _ = self.consume();
                            const step_t = try self.expect(.Number);
                            step = try std.fmt.parseInt(i64, step_t.value, 10);
                        }

                        _ = try self.expect(.RBracket);

                        if (step <= 0) return error.InvalidCharacter;
                        const count = @divTrunc((end_v - start_v), step) + 1;
                        if (count <= 0) return error.InvalidCharacter;

                        var items = try self.allocator.alloc(i64, @intCast(count));
                        var value = start_v;
                        for (0..@intCast(count)) |i| {
                            items[i] = value;
                            value += step;
                        }

                        return try self.alloc_node(.{ .list = items });
                    } else {
                        var items: ArrayList(i64) = .empty;
                        const first = try std.fmt.parseInt(i64, start.value, 10);
                        try items.append(self.allocator, first);

                        while (self.peek().type != .RBracket) {
                            if (self.peek().type == .Comma) _ = self.consume();
                            if (self.peek().type == .RBracket) break; // ^this caused a lot of headache...

                            const value = try std.fmt.parseInt(i64, (try self.expect(.Number)).value, 10);
                            try items.append(self.allocator, value);
                        }

                        _ = try self.expect(.RBracket);
                        return try self.alloc_node(.{ .list = try items.toOwnedSlice(self.allocator) });
                    }
                }

                _ = try self.expect(.RBracket);
                return try self.alloc_node(.{ .list = try self.allocator.alloc(i64, 0) });
            },
            .LParen => {
                const next = self.peek();

                const is_op = switch (next.type) {
                    .Add, .Sub, .Mul, .Div => true,
                    .Symbol => get_intrinsic(next.value) == null,
                    else => false,
                };

                if (is_op) {
                    const optk = self.consume();
                    const arg = try self.parse_expr(0);

                    _ = try self.expect(.RParen);

                    return try self.alloc_node(.{ .partial = .{ .op = optk.value, .arg = arg } });
                } else {
                    const node = try self.parse_expr(0);

                    _ = try self.expect(.RParen);

                    return node;
                }
            },

            else => error.UnexpectedToken,
        };
    }

    fn parse_left_deno(self: *Parser, left: *Node) ParserError!*Node {
        // the only infix op is an application, left associative.
        const right = try self.parse_expr(10);
        return try self.alloc_node(.{ .application = .{ .func = left, .arg = right } });
    }

    fn peek(self: *Parser) Token {
        if (self.pos >= self.tokens.items.len) return .{ .type = .EOF, .value = "" };
        return self.tokens.items[self.pos];
    }

    fn consume(self: *Parser) Token {
        const prev = self.peek();
        if (prev.type != .EOF) self.pos += 1;
        return prev;
    }

    fn expect(self: *Parser, typ: TokenType) error{UnexpectedToken}!Token {
        const prev = self.consume();
        if (prev.type != typ) {
            std.debug.print("expected {}, got {}\n", .{ typ, prev.type });
            return error.UnexpectedToken;
        }
        return prev;
    }

    fn alloc_node(self: *Parser, node: Node) !*Node {
        const n = try self.allocator.create(Node);
        n.* = node;
        return n;
    }

    fn get_precedence(self: *Parser, token: Token) u8 {
        _ = self;

        return switch (token.type) {
            .Number, .String, .LParen, .LBracket => 10, // previous expression is being applied to these tokens as they start a new expr
            else => 0,
        };
    }
};

const EvalError = error{ Unreachable, TypeError, OutOfBounds };

fn eval(allocator: std.mem.Allocator, node: *Node) !Value {
    switch (node.*) {
        .literal => |i| return Value{ .integer = i },
        .list => |l| return Value{ .list = l },
        .string => |s| return Value{ .string = s },
        .char => |c| return Value{ .char = c },
        .partial => return Value{ .function = node },
        .composition => return Value{ .function = node },
        .intrinsic => return Value{ .function = node },
        .application => |a| {
            const fun_val = try eval(allocator, a.func);
            const arg_val = try eval(allocator, a.arg);

            if (fun_val != .function) return EvalError.TypeError;
            const fun_node = fun_val.function;

            switch (fun_node.*) {
                .partial => |p| {
                    const part_arg_val = try eval(allocator, p.arg);
                    if (part_arg_val != .integer) return EvalError.TypeError;

                    const lhv = part_arg_val.integer;

                    if (arg_val != .integer) return EvalError.TypeError;

                    const rhv = arg_val.integer;
                    return Value{ .integer = switch (p.op[0]) {
                        '+' => lhv + rhv,
                        '-' => rhv - lhv,
                        '*' => lhv * rhv,
                        '/' => @divTrunc(lhv, rhv),
                        else => return EvalError.TypeError,
                    } };
                },

                .composition => |c| {
                    const inner_a = try allocator.create(Node);
                    inner_a.* = .{ .application = .{ .func = c.inner, .arg = a.arg } };
                    const inner_result = try eval(allocator, inner_a);

                    const result_node = try allocator.create(Node);
                    switch (inner_result) {
                        .integer => |i| result_node.* = .{ .literal = i },
                        .list => |l| result_node.* = .{ .list = l },
                        .string => |s| result_node.* = .{ .string = s },
                        .char => |ch| result_node.* = .{ .char = ch },
                        .function => |f| result_node.* = f.*,
                        .unit => {},
                    }

                    const outer_a = try allocator.create(Node);
                    outer_a.* = .{ .application = .{ .func = c.outer, .arg = result_node } };
                    return try eval(allocator, outer_a);
                },

                .intrinsic => |i| {
                    const intrinsic_f = get_intrinsic(i.name) orelse return EvalError.TypeError;

                    var n_args: ArrayList(*Node) = .empty;
                    for (i.args) |n| {
                        try n_args.append(allocator, n);
                    }

                    const a_node = try allocator.create(Node);
                    switch (arg_val) {
                        .integer => |int| a_node.* = .{ .literal = int },
                        .list => |lst| a_node.* = .{ .list = lst },
                        .string => |s| a_node.* = .{ .string = s },
                        .char => |ch| a_node.* = .{ .char = ch },
                        .function => |f| a_node.* = f.*,
                        .unit => {},
                    }

                    try n_args.append(allocator, a_node);

                    var e_args: ArrayList(Value) = .empty;
                    for (n_args.items) |n| {
                        try e_args.append(allocator, try eval(allocator, n));
                    }

                    const result = intrinsic_f.func(allocator, e_args.items) catch {
                        const n_node = try allocator.create(Node);
                        n_node.* = .{ .intrinsic = .{ .name = i.name, .args = try n_args.toOwnedSlice(allocator) } };

                        return Value{ .function = n_node };
                    };

                    return result;
                },

                else => return EvalError.TypeError,
            }
        },
    }
}

fn log_error(writer: *std.io.Writer, error_type: []const u8, message: []const u8) !void {
    try writer.print("\x1b[38;5;211m{s} {s}:\x1b[0m {s}\n", .{ "\u{26A0}", error_type, message });
}

pub fn main() !void {
    if (builtin.os.tag == .windows) {
        const win = std.os.windows;
        _ = win.kernel32.SetConsoleOutputCP(65001);
    }

    var stdout_buf: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buf);
    const stdout: *std.io.Writer = &stdout_writer.interface;

    var stdin_buf: [1024]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buf);
    const stdin: *std.io.Reader = &stdin_reader.interface;

    const v = "version 1.1.1";
    const reset = "\x1b[0m";

    try stdout.print("\x1b[38;2;255;179;186m" ++ "  _                 \n", .{});
    try stdout.print("\x1b[38;2;255;223;186m" ++ " | |                \n", .{});
    try stdout.print("\x1b[38;2;255;255;186m" ++ " | | __ _ _ __ ___    \n", .{});
    try stdout.print("\x1b[38;2;186;255;201m" ++ " | |/ _` | '_ ` _ \\      {s}\n", .{v});
    try stdout.print("\x1b[38;2;186;225;255m" ++ " | | (_| | | | | | |\n", .{});
    try stdout.print("\x1b[38;2;223;186;255m" ++ " |_|\\__,_|_| |_| |_|\n", .{});
    try stdout.print(reset ++ "\n", .{});
    try stdout.print("\n", .{});

    repl: while (true) {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();
        const allocator = arena.allocator();

        try stdout.print("\x1b[38;5;219m{s}\x1b[0m ", .{"\u{03bb}"});
        try stdout.flush();

        const raw_line = try stdin.takeDelimiter('\n') orelse unreachable;
        const line = std.mem.trim(u8, raw_line, "\r");

        if (std.mem.eql(u8, line, "exit")) break :repl;

        var parser: Parser = Parser.init(allocator, line) catch |err| {
            switch (err) {
                error.UnterminatedStringLiteral => try log_error(stdout, "lexer error", "unterminated string literal"),
                error.InvalidCharacterLiteral => try log_error(stdout, "lexer error", "character literals must contain one alphanumeric character"),
                else => try log_error(stdout, "error", @errorName(err)),
            }
            continue :repl;
        };

        while (true) {
            const n = parser.parse() catch |err| {
                try log_error(stdout, "parse error", @errorName(err));
                continue :repl;
            };

            const node = n orelse break;
            // std.debug.print("parsed a node\n", .{});
            const result = eval(allocator, node) catch |err| {
                try log_error(stdout, "eval error", @errorName(err));
                continue :repl;
            };

            if (result != .unit) {
                try result.fmt("{}\n", .{}, stdout);
            }

            try stdout.flush();
        }
    }
}

fn intrinsic_map(allocator: std.mem.Allocator, args: []const Value) !Value {
    if (args.len != 2) return EvalError.TypeError;
    if (args[0] != .function) return EvalError.TypeError;
    if (args[1] != .list) return EvalError.TypeError;

    const func = args[0].function;
    const list = args[1].list;

    var result_list = try allocator.alloc(i64, list.len);
    for (list, 0..) |item, idx| {
        const i_node = try allocator.create(Node);
        i_node.* = .{ .literal = item };

        const a_node = try allocator.create(Node);
        a_node.* = .{ .application = .{ .func = func, .arg = i_node } };

        const i_result = try eval(allocator, a_node);
        if (i_result != .integer) return EvalError.TypeError;

        result_list[idx] = i_result.integer;
    }

    return Value{ .list = result_list };
}

fn intrinsic_get(allocator: std.mem.Allocator, args: []const Value) !Value {
    _ = allocator;

    if (args.len != 2) return EvalError.TypeError;
    if (args[0] != .integer) return EvalError.TypeError;
    if (args[1] != .list and args[1] != .string) return EvalError.TypeError;

    const idx = args[0].integer;
    switch (args[1]) {
        .list => |list| {
            if (idx < 0 or idx >= list.len) return EvalError.OutOfBounds;
            return Value{ .integer = list[@intCast(idx)] };
        },

        .string => |str| {
            if (idx < 0 or idx >= str.len) return EvalError.OutOfBounds;
            return Value{ .char = str[@intCast(idx)] };
        },

        else => return EvalError.TypeError,
    }
}

fn intrinsic_putln(allocator: std.mem.Allocator, args: []const Value) !Value {
    _ = allocator;
    if (args.len != 1) return EvalError.TypeError;
    if (args[0] != .string) return EvalError.TypeError;

    std.debug.print("{s}\n", .{args[0].string});

    return Value{ .unit = {} };
}

fn intrinsic_str(allocator: std.mem.Allocator, args: []const Value) !Value {
    if (args.len != 1) return EvalError.TypeError;
    if (args[0] != .list) return EvalError.TypeError;

    const list = args[0].list;
    const result = try allocator.alloc(u8, list.len);
    for (list, 0..) |n, i| {
        if (n < 0 or n > std.math.maxInt(u8)) return EvalError.TypeError;
        result[i] = @intCast(n);
    }

    return Value{ .string = result };
}

fn intrinsic_chars(allocator: std.mem.Allocator, args: []const Value) !Value {
    if (args.len != 1) return EvalError.TypeError;
    if (args[0] != .string) return EvalError.TypeError;

    const str = args[0].string;
    const result = try allocator.alloc(i64, str.len);
    for (str, 0..) |c, i| {
        result[i] = c;
    }

    return Value{ .list = result };
}

fn intrinsic_compose(allocator: std.mem.Allocator, args: []const Value) !Value {
    if (args.len != 2) return EvalError.TypeError;
    if (args[0] != .function or args[1] != .function) return EvalError.TypeError;

    const c_node = try allocator.create(Node);
    c_node.* = .{ .composition = .{ .outer = args[0].function, .inner = args[1].function } };

    return Value{ .function = c_node };
}

fn intrinsic_list(allocator: std.mem.Allocator, args: []const Value) !Value {
    if (args.len != 2) return EvalError.TypeError;
    if (args[0] != .integer or args[1] != .integer) return EvalError.TypeError;

    const val = args[0].integer;
    const count = args[1].integer;

    if (count < 0) return EvalError.TypeError;

    const res = try allocator.alloc(i64, @intCast(count));
    @memset(res, val);

    return Value{ .list = res };
}
