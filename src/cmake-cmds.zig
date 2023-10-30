const std = @import("std");
const assert = std.debug.assert;
const arg_parse = @import("arg-parse.zig");

const DepName = @import("shared.zig").DepName;
const CmakeOptimize = @import("shared.zig").CmakeOptimize;

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var cmd_args_hm: std.StringHashMapUnmanaged(?[]const u8) = .{};
    defer {
        var it = cmd_args_hm.iterator();
        while (it.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            allocator.free(entry.value_ptr.* orelse &[_]u8{});
        }
        cmd_args_hm.deinit(allocator);
    }
    try arg_parse.parseOsArgsHashMap(allocator, &cmd_args_hm);

    const target_os_tag: std.Target.Os.Tag = std.meta.stringToEnum(
        std.Target.Os.Tag,
        (cmd_args_hm.get("os-tag") orelse return error.MissingOsTagArg) orelse
            return error.MissingOsTagArgValue,
    ) orelse return error.UnrecognizedOsTagArgValue;

    const verbose: bool = if (std.meta.stringToEnum(
        enum { false, true },
        (try (cmd_args_hm.get("verbose") orelse error.MissingVerboseFlag)) orelse "true",
    )) |bool_tag| switch (bool_tag) {
        .false => false,
        .true => true,
    } else return error.NonBooleanVerboseFlag;

    const optimze: CmakeOptimize = std.meta.stringToEnum(
        CmakeOptimize,
        (try (cmd_args_hm.get("optimize") orelse error.MissingOptimizeArg)) orelse "true",
    ) orelse return error.UnrecognizedOptimizeArgValue;

    const cmake_exe: []const u8 = (cmd_args_hm.get("cmake-exe") orelse
        return error.MissingCmakeExePathArg) orelse {
        return error.MissingCmakeExePathArgValue;
    };

    const build_dir_path: []const u8 = (cmd_args_hm.get("build-dir") orelse
        return error.MissingOutputDirArg) orelse {
        return error.MissingOutputDirArgValue;
    };
    assert(std.fs.path.isAbsolute(build_dir_path));

    const install_dir_path = (cmd_args_hm.get("install-dir") orelse
        return error.MissingInstallDirArg) orelse {
        return error.MissingInstallDirArgValue;
    };
    assert(std.fs.path.isAbsolute(install_dir_path));

    const DepPaths = std.EnumMap(DepName, []const u8);
    var dep_paths: DepPaths = .{};
    inline for (comptime std.enums.values(DepName)) |dname| cont: {
        const cmd_arg_name = "dep-" ++ @tagName(dname);

        if (!dname.neededFor(target_os_tag)) {
            if (cmd_args_hm.contains(cmd_arg_name)) {
                std.log.err("Shoud not specify dependency path for {s} given configuration", .{@tagName(dname)});
            }
            break :cont;
        }
        const path = cmd_args_hm.get(cmd_arg_name) orelse {
            std.log.err("Must specify dependency path for {s} with '--{s}'", .{ @tagName(dname), cmd_arg_name });
            return error.MissingDependencyPath;
        } orelse {
            std.log.err("Must specify value for '{s}'", .{cmd_arg_name});
            return error.NonStringDependencyArg;
        };

        assert(dep_paths.fetchPut(dname, path) == null);
    }

    var dep_path_iter = dep_paths.iterator();

    var path_list_buf = std.ArrayList([]const u8).init(allocator);
    defer path_list_buf.deinit();

    var argv_buf = std.ArrayList([]const u8).init(allocator);
    defer argv_buf.deinit();

    var loop_arena_state = std.heap.ArenaAllocator.init(allocator);
    defer loop_arena_state.deinit();
    const loop_arena = loop_arena_state.allocator();

    while (dep_path_iter.next()) |dep_entry| {
        path_list_buf.clearRetainingCapacity();
        for (0..3) |_| if (loop_arena_state.reset(.retain_capacity)) break;
        const source_path = dep_entry.value.*;
        const build_path = try std.fs.path.join(loop_arena, &.{ build_dir_path, @tagName(dep_entry.key) });

        var iterable_dir = try std.fs.cwd().openIterableDir(source_path, .{});
        defer iterable_dir.close();

        var walker = try iterable_dir.walk(allocator);
        defer walker.deinit();

        while (try walker.next()) |entry| {
            switch (entry.kind) {
                else => continue,
                .file => {},
            }
            const dirname = std.fs.path.dirname(entry.path);
            const ext = std.fs.path.extension(entry.path);
            const basename = std.fs.path.basename(entry.path);
            std.debug.assert(std.mem.eql(u8, basename, entry.basename));

            switch (dep_entry.key) {
                .@"aws-lc" => {
                    if (dirname != null and
                        !std.mem.startsWith(u8, dirname.?, "include") and
                        !std.mem.startsWith(u8, dirname.?, "ssl") and
                        !std.mem.startsWith(u8, dirname.?, "crypto") and
                        !std.mem.startsWith(u8, dirname.?, "generated-src") //
                    ) continue;
                    if (!std.mem.eql(u8, ext, ".c") and
                        !std.mem.eql(u8, ext, ".h") and
                        !std.mem.eql(u8, ext, ".s") and
                        !std.mem.eql(u8, ext, ".S") and
                        !std.mem.eql(u8, basename, "CMakeLists.txt") //
                    ) continue;
                },
                .@"s2n-tls" => {
                    if (dirname != null and
                        !std.mem.startsWith(u8, dirname.?, "api") and
                        !std.mem.startsWith(u8, dirname.?, "crypto") and
                        !std.mem.startsWith(u8, dirname.?, "stuffer") and
                        !std.mem.startsWith(u8, dirname.?, "tls") and
                        !std.mem.startsWith(u8, dirname.?, "pq-crypto") and
                        !std.mem.startsWith(u8, dirname.?, "utils") //
                    ) continue;
                    if (!std.mem.eql(u8, ext, ".c") and
                        !std.mem.eql(u8, ext, ".h") and
                        !std.mem.eql(u8, basename, "CMakeLists.txt") //
                    ) continue;
                },
                else => {
                    if (dirname != null and
                        !std.mem.startsWith(u8, dirname.?, "source") and
                        !std.mem.startsWith(u8, dirname.?, "include") //
                    ) continue;
                    if (!std.mem.eql(u8, ext, ".c") and
                        !std.mem.eql(u8, ext, ".h") and
                        !std.mem.eql(u8, basename, "CMakeLists.txt") //
                    ) continue;
                },
            }

            const entry_path = try std.fs.path.join(loop_arena, &.{ source_path, entry.path });
            try path_list_buf.append(entry_path);
        }

        std.sort.block([]const u8, path_list_buf.items, void{}, struct {
            fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
                return std.mem.lessThan(u8, lhs, rhs);
            }
        }.lessThan);

        { // configure
            argv_buf.clearRetainingCapacity();

            try argv_buf.append(cmake_exe);
            try argv_buf.appendSlice(&.{ "-S", source_path });
            try argv_buf.appendSlice(&.{ "-B", build_path });
            try argv_buf.appendSlice(&.{ "-G", "Ninja" });

            try argv_buf.append(try std.fmt.allocPrint(loop_arena, "-DCMAKE_BUILD_TYPE={s}", .{@tagName(optimze)}));
            try argv_buf.append(try std.fmt.allocPrint(loop_arena, "-DCMAKE_INSTALL_PREFIX={s}", .{install_dir_path}));
            try argv_buf.append(try std.fmt.allocPrint(loop_arena, "-DCMAKE_PREFIX_PATH={s}", .{install_dir_path}));

            try argv_buf.append("-DBUILD_TESTING=" ++ "OFF");

            switch (dep_entry.key) {
                .@"aws-lc" => {
                    try argv_buf.append("-DDISABLE_GO=" ++ "ON");
                    try argv_buf.append("-DDISABLE_PERL=" ++ "ON");
                    try argv_buf.append("-DBUILD_TOOL=" ++ "OFF"); // TODO: I assume this disables outputting the executables
                },
                .@"s2n-tls" => {
                    try argv_buf.append("-DSEARCH_LIBCRYPTO=" ++ "OFF"); // TODO: no idea what this does but doesn't break anything
                },

                .@"aws-c-common" => {},
                .@"aws-checksums" => {},
                .@"aws-c-cal" => {},
                .@"aws-c-io" => {},
                .@"aws-c-compression" => {},
                .@"aws-c-http" => {},
                .@"aws-c-sdkutils" => {},
                .@"aws-c-auth" => {},

                .@"aws-c-s3" => {},
            }

            if (verbose) {
                const text = try std.Build.Step.allocPrintCmd(loop_arena, null, argv_buf.items);
                defer loop_arena.free(text);
                std.log.info("{s}", .{text});
            }
            var config_cp = std.ChildProcess.init(argv_buf.items, loop_arena);
            switch (try config_cp.spawnAndWait()) {
                .Exited => |code| {
                    if (code != 0) {
                        const text = try std.Build.Step.allocPrintCmd(loop_arena, null, argv_buf.items);
                        std.log.err("the following command exited with error code {d}:\n{s}", .{ code, text });
                        return error.NonZeroExitCode;
                    }
                },
                .Signal, .Stopped, .Unknown => {
                    const text = try std.Build.Step.allocPrintCmd(loop_arena, null, argv_buf.items);
                    std.log.err("the following command terminated unexpectedly:\n{s}", .{text});
                    return error.UnexpectedTermination;
                },
            }
        }

        { // install
            argv_buf.clearRetainingCapacity();

            try argv_buf.append(cmake_exe);
            try argv_buf.appendSlice(&.{ "--build", build_path });
            try argv_buf.appendSlice(&.{ "--target", "install" });

            if (verbose) {
                const text = try std.Build.Step.allocPrintCmd(loop_arena, null, argv_buf.items);
                defer loop_arena.free(text);
                std.log.info("{s}", .{text});
            }
            var install_cp = std.ChildProcess.init(argv_buf.items, loop_arena);
            switch (try install_cp.spawnAndWait()) {
                .Exited => |code| {
                    if (code != 0) {
                        const text = try std.Build.Step.allocPrintCmd(loop_arena, null, argv_buf.items);
                        std.log.err("the following command exited with error code {d}:\n{s}", .{ code, text });
                        return error.NonZeroExitCode;
                    }
                },
                .Signal, .Stopped, .Unknown => {
                    const text = try std.Build.Step.allocPrintCmd(loop_arena, null, argv_buf.items);
                    std.log.err("the following command terminated unexpectedly:\n{s}", .{text});
                    return error.UnexpectedTermination;
                },
            }
        }
    }
}
