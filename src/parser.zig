const std = @import("std");
const types = @import("types.zig");
const rt = @import("rt.zig");
const get_intrinsic = rt.get_intrinsic;
const ArrayList = std.ArrayList;
const Token = types.Token;
const TokenType = types.TokenType;
const Node = types.Node;

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

pub const Parser = struct {
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
