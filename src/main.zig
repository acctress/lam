const std = @import("std");
const builtin = @import("builtin");
const rt = @import("rt.zig");
const Parser = @import("parser.zig").Parser;
const ArrayList = std.ArrayList;

const LAM_VERSION: []const u8 = "version 1.1.2";

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
    const reset = "\x1b[0m";

    try stdout.print("\x1b[38;2;255;179;186m" ++ "  _                 \n", .{});
    try stdout.print("\x1b[38;2;255;223;186m" ++ " | |                \n", .{});
    try stdout.print("\x1b[38;2;255;255;186m" ++ " | | __ _ _ __ ___    \n", .{});
    try stdout.print("\x1b[38;2;186;255;201m" ++ " | |/ _` | '_ ` _ \\      {s}\n", .{LAM_VERSION});
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
            const result = rt.eval(allocator, node) catch |err| {
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
