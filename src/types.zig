const std = @import("std");

pub const TokenType = enum(u8) {
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

pub const Token = struct {
    type: TokenType,
    value: []const u8,
};

pub const ValueType = enum { integer, list, string, char, function, unit }; // unit is void but void is a keyword already so cant us it

pub const Value = union(ValueType) {
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

pub const IntrinsicFN = *const fn (allocator: std.mem.Allocator, args: []const Value) anyerror!Value;
pub const Intrinsic = struct { name: []const u8, func: IntrinsicFN };

pub const NodeType = enum(u8) { partial, application, literal, list, string, char, composition, intrinsic };

pub const Node = union(NodeType) {
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
