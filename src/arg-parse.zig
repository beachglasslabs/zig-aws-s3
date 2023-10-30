const std = @import("std");
const assert = std.debug.assert;

pub fn parseArgsCtx(
    /// Methods:
    /// * `pub fn nextToken(ctx) ?[]const u8`:
    ///   Returns the next argv string. It is presumed that each returned string
    ///   is only valid until the next call to this function.
    ///
    /// * `pub fn put(ctx, name: []const u8) !void`:
    ///   Add the argument with the given name to the output.
    ///
    /// * `pub fn set(ctx, value: []const u8) !void`:
    ///   Only ever called after `ctx.put(...)`, and never consecutively.
    ///   Sets the value of the last argument which was `put`.
    ///
    /// * `pub fn addPositional(ctx, positional: []const u8) !void`:
    ///   Add the positional string argument to the output.
    ///
    /// * `pub fn extraSeparator() void`:
    ///   Informs the context that the "--" string has been encountered,
    ///   immediately before returning.
    ///   This can be used to set a flag to indicate that `arg_iter`
    ///   may return arbitrary trailing arguments.
    ctx: anytype,
) !void {
    var cached_tok: ?[]const u8 = null;

    while (true) {
        const tok_1: []const u8 = cached_tok orelse ctx.nextToken() orelse break;
        cached_tok = null;

        const prefix = "--";
        if (!std.mem.startsWith(u8, tok_1, prefix)) {
            try ctx.addPositional(tok_1);
            continue;
        }

        const eql_idx = std.mem.indexOfScalarPos(u8, tok_1, prefix.len, '=') orelse {
            { // check for name or extra separator
                const name = std.mem.trim(u8, tok_1[prefix.len..], &std.ascii.whitespace);
                if (name.len == 0) {
                    try ctx.extraSeparator();
                    break;
                }
                try ctx.put(name);
            }

            const tok_2 = ctx.nextToken() orelse break;

            if (!std.mem.startsWith(u8, tok_2, prefix)) {
                try ctx.set(tok_2);
                continue;
            }

            if (std.mem.trim(u8, tok_2[prefix.len..], &std.ascii.whitespace).len == 0) {
                try ctx.extraSeparator();
                break;
            }

            cached_tok = tok_2;
            continue;
        };

        const name = std.mem.trim(u8, tok_1[prefix.len..eql_idx], &std.ascii.whitespace);
        const value = tok_1[eql_idx + 1 ..];
        if (name.len == 0) {
            try ctx.extraSeparator();
            break;
        }
        try ctx.put(name);
        try ctx.set(value);
    }
}

pub fn parseOsArgsHashMap(
    allocator: std.mem.Allocator,
    /// `*std.StringHashMapUnmanaged(?[]const u8)` | `*std.StringArrayHashMapUnmanaged(?[]const u8)`
    cmd_args_ptr: anytype,
) !void {
    assert(cmd_args_ptr.count() == 0);
    var arg_iter = try std.process.argsWithAllocator(allocator);
    defer arg_iter.deinit();
    _ = arg_iter.next() orelse return error.EmptyArgvMissingExePath;
    try parseArgsHashMap(allocator, cmd_args_ptr, &arg_iter);
}

pub fn parseArgsHashMap(
    allocator: std.mem.Allocator,
    /// `*std.StringHashMapUnmanaged(?[]const u8)` | `*std.StringArrayHashMapUnmanaged(?[]const u8)`
    cmd_args_ptr: anytype,
    arg_iter: anytype,
) !void {
    assert(cmd_args_ptr.count() == 0);

    var ctx: struct {
        pub fn nextToken(ctx: @This()) ?[]const u8 {
            return ctx.arg_iter.next();
        }

        pub fn put(ctx: *@This(), arg_name: []const u8) !void {
            const gop = try ctx.cmd_args.getOrPut(ctx.allocator, arg_name);
            if (gop.found_existing) {
                std.log.err("Specified arg '{s}' multiple times", .{arg_name});
                return error.DuplicateArg;
            }

            gop.key_ptr.* = try ctx.allocator.dupe(u8, arg_name);
            gop.value_ptr.* = null;

            ctx.last = gop.key_ptr.*;
        }

        pub fn set(ctx: *@This(), arg_value: []const u8) !void {
            const ptr = ctx.cmd_args.getPtr(ctx.last.?).?;
            ctx.last = null;

            assert(ptr.* == null);
            ptr.* = try ctx.allocator.dupe(u8, arg_value);
        }

        pub fn addPositional(_: @This(), positional: []const u8) !void {
            std.log.err("Unexpected positional argument '{s}'", .{positional});
            return error.UnexpectedPositional;
        }

        pub fn extraSeparator(_: @This()) !void {
            std.log.err("Unexpected trailing arguments indicated by '--'", .{});
            return error.UnexpectedTrailingArguments;
        }

        last: ?[]const u8 = null,

        allocator: @TypeOf(allocator),
        cmd_args: @TypeOf(cmd_args_ptr),
        arg_iter: @TypeOf(arg_iter),
    } = .{
        .allocator = allocator,
        .cmd_args = cmd_args_ptr,
        .arg_iter = arg_iter,
    };
    try parseArgsCtx(&ctx);
}
