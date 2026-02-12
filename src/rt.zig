const std = @import("std");
const types = @import("types.zig");
const ArrayList = std.ArrayList;
const Value = types.Value;
const Intrinsic = types.Intrinsic;
const Node = types.Node;

const INTRINSICS = [_]Intrinsic{
    .{ .name = "map", .func = intrinsic_map },
    .{ .name = "compose", .func = intrinsic_compose },
    .{ .name = "list", .func = intrinsic_list },
    .{ .name = "get", .func = intrinsic_get },
    .{ .name = "putln", .func = intrinsic_putln },
    .{ .name = "str", .func = intrinsic_str },
    .{ .name = "chars", .func = intrinsic_chars },
};

pub fn get_intrinsic(name: []const u8) ?Intrinsic {
    for (INTRINSICS) |i| {
        if (std.mem.eql(u8, i.name, name)) return i;
    }

    return null;
}

pub const EvalError = error{ Unreachable, TypeError, OutOfBounds };

pub fn eval(allocator: std.mem.Allocator, node: *Node) !Value {
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
