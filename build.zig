const std = @import("std");
const Build = std.Build;

pub fn awsIncludeDir(
    zig_aws_s3_dep: *Build.Dependency,
) Build.LazyPath {
    const b = zig_aws_s3_dep.builder;
    const prefix = zig_aws_s3_dep.module(module_hack_name).source_file;
    return relativeLazyPath(b, prefix, "include");
}
pub fn linkAwsLibs(
    comp: *Build.Step.Compile,
    zig_aws_s3_dep: *Build.Dependency,
) void {
    const b = zig_aws_s3_dep.builder;
    const prefix = zig_aws_s3_dep.module(module_hack_name).source_file;
    for (aws_libs_names) |libname| {
        switch (comp.target.getOsTag()) {
            .linux => {},
            else => {
                if (std.mem.eql(u8, libname, "crypto") or
                    std.mem.eql(u8, libname, "s2n") or
                    std.mem.eql(u8, libname, "ssl"))
                    continue;
            },
        }
        comp.addObjectFile(relativeLazyPath(b, prefix, b.fmt("lib" ++ std.fs.path.sep_str ++ "lib{s}.a", .{libname})));
    }
}

comptime {
    _ = &awsIncludeDir;
    _ = &linkAwsLibs;
}

const module_hack_name = blk: {
    // zig fmt: off
    const file = struct { fn file() []const u8 { return @src().file; } }.file();
    // zig fmt: on

    const SipHash = std.hash.SipHash128(1, 2);
    var hasher = SipHash.init(&[_]u8{0xAB} ** SipHash.key_length);
    const file_bytes = @embedFile(file);

    @setEvalBranchQuota(file_bytes.len * 1000);
    hasher.update(file_bytes);

    break :blk &std.fmt.bytesToHex(hasher.finalResult(), .lower);
};

const aws_libs_names = [_][]const u8{
    "aws-c-auth",
    "aws-c-cal",
    "aws-c-common",
    "aws-c-compression",
    "aws-checksums",
    "aws-c-http",
    "aws-c-io",
    "aws-c-s3",
    "aws-c-sdkutils",
    "crypto",
    "s2n",
    "ssl",
};

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const cmake_exe_path = b.option([]const u8, "cmake_exe", "Specifies the CMake executable to invoke for build");

    // linux-only

    const lc_dep = b.dependency("aws/aws-lc", .{});
    const s2n_tls_dep = b.dependency("aws/s2n-tls", .{});

    // remaining

    const c_common_dep = b.dependency("awslabs/aws-c-common", .{});
    const checksums_dep = b.dependency("awslabs/aws-checksums", .{});
    const c_cal_dep = b.dependency("awslabs/aws-c-cal", .{});
    const c_io_dep = b.dependency("awslabs/aws-c-io", .{});
    const c_compression_dep = b.dependency("awslabs/aws-c-compression", .{});
    const c_http_dep = b.dependency("awslabs/aws-c-http", .{});
    const c_sdkutils_dep = b.dependency("awslabs/aws-c-sdkutils", .{});
    const c_auth_dep = b.dependency("awslabs/aws-c-auth", .{});

    // the main dependency

    const c_s3_dep = b.dependency("awslabs/aws-c-s3", .{});

    const aws_cmake_install_dir: Build.LazyPath = cmake: {
        const exe_path = cmake_exe_path orelse {
            const poison_dir = b.allocator.create(Build.GeneratedFile) catch |e| @panic(@errorName(e));
            poison_dir.* = .{ .step = failureStep(b, error.CmakeExePathUnspecified, "Must specify cmake_exe to use CMake build") };
            break :cmake .{ .generated = poison_dir };
        };

        const aws_cmake_step = AwsCMakeStep.create(
            b,
            exe_path,
            target.getOsTag(),
            AwsCMakeStep.Optimize.from(optimize),
            .{
                .lc = lc_dep.path(""),
                .s2n_tls = s2n_tls_dep.path(""),

                .c_common = c_common_dep.path(""),
                .checksums = checksums_dep.path(""),
                .c_cal = c_cal_dep.path(""),
                .c_io = c_io_dep.path(""),
                .c_compression = c_compression_dep.path(""),
                .c_http = c_http_dep.path(""),
                .c_sdkutils = c_sdkutils_dep.path(""),
                .c_auth = c_auth_dep.path(""),

                .c_s3 = c_s3_dep.path(""),
            },
        );
        break :cmake aws_cmake_step.installPrefix();
    };
    _ = b.addModule(module_hack_name, .{ .source_file = aws_cmake_install_dir });
}

pub const AwsCMakeStep = struct {
    step: Build.Step,
    generated_install: Build.GeneratedFile,

    cmake_exe: []const u8,
    os_tag: std.Target.Os.Tag,
    optimize: Optimize,
    source_dirs: SourceDirs,

    pub const id: Build.Step.Id = .custom;

    pub fn create(
        b: *Build,
        cmake_exe: []const u8,
        os_tag: std.Target.Os.Tag,
        optimize: Optimize,
        source_dirs: SourceDirs,
    ) *AwsCMakeStep {
        const self = b.allocator.create(AwsCMakeStep) catch |e| @panic(@errorName(e));
        self.* = .{
            .step = Build.Step.init(.{
                .id = AwsCMakeStep.id,
                .name = b.fmt("Build AWS {s} with {s} for {s}", .{ @tagName(optimize), cmake_exe, @tagName(os_tag) }),
                .owner = b,
                .makeFn = AwsCMakeStep.make,
            }),
            .generated_install = .{ .step = &self.step },

            .cmake_exe = b.dupe(cmake_exe),
            .os_tag = os_tag,
            .optimize = optimize,
            .source_dirs = source_dirs,
        };
        return self;
    }

    pub fn installPrefix(self: *AwsCMakeStep) Build.LazyPath {
        return .{ .generated = &self.generated_install };
    }

    pub const SourceDirs = std.enums.EnumFieldStruct(DepId, Build.LazyPath, null);
    const DepId = enum {
        // linux-only
        lc,
        s2n_tls,

        // remaining
        c_common,
        checksums,
        c_cal,
        c_io,
        c_compression,
        c_http,
        c_sdkutils,
        c_auth,

        // the mainendency
        c_s3,
    };
    pub const Optimize = enum {
        Debug,
        MinSizeRel,
        Release,
        RelWithDebInfo,

        pub inline fn from(optimize: std.builtin.Mode) Optimize {
            return switch (optimize) {
                .Debug => .Debug,
                .ReleaseSmall => .MinSizeRel,
                .ReleaseFast => .Release,
                .ReleaseSafe => .RelWithDebInfo,
            };
        }
    };

    fn mustSkip(self: *AwsCMakeStep, dep: DepId) bool {
        return switch (dep) {
            .lc, .s2n_tls => switch (self.os_tag) {
                else => true,
                .linux => false,
            },
            else => false,
        };
    }

    fn make(step: *Build.Step, prog: *std.Progress.Node) anyerror!void {
        _ = prog;
        const self = @fieldParentPtr(AwsCMakeStep, "step", step);
        const b = step.owner;

        var man = b.cache.obtain();
        defer man.deinit();

        man.hash.addBytes(self.cmake_exe);
        man.hash.addBytes(std.mem.asBytes(&self.optimize));

        inline for (@typeInfo(DepId).Enum.fields) |field| skip: {
            const dep_id: DepId = @field(DepId, field.name);
            if (self.mustSkip(dep_id)) break :skip;

            const source_path = Build.LazyPath.getPath2(@field(self.source_dirs, field.name), b, step);

            var path_list = std.ArrayList([]const u8).init(b.allocator);
            defer path_list.deinit();

            var iterable_dir = try b.build_root.handle.openIterableDir(source_path, .{});
            defer iterable_dir.close();

            var walker = try iterable_dir.walk(b.allocator);
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

                switch (dep_id) {
                    .lc => {
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
                    .s2n_tls => {
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

                try path_list.append(b.pathJoin(&.{ source_path, entry.path }));
            }

            std.sort.block([]const u8, path_list.items, void{}, struct {
                fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
                    return std.mem.lessThan(u8, lhs, rhs);
                }
            }.lessThan);
            try man.addListOfFiles(path_list.items);
        }

        const hit = try step.cacheHit(&man);
        const digest = man.final();
        const rel_cache_path = "o".* ++ std.fs.path.sep_str.* ++ digest;
        const install_dir_path = try b.cache_root.join(b.allocator, &.{ &rel_cache_path, "install" });
        self.generated_install.path = install_dir_path;
        if (hit) return;

        const rel_build_path_base = rel_cache_path ++ std.fs.path.sep_str.* ++ "build".*;

        var argv_buf = std.ArrayList([]const u8).init(b.allocator);
        defer argv_buf.deinit();

        inline for (@typeInfo(DepId).Enum.fields) |field| skip: {
            const dep_id: DepId = @field(DepId, field.name);
            if (self.mustSkip(dep_id)) break :skip;

            const source_path = Build.LazyPath.getPath2(@field(self.source_dirs, field.name), b, step);
            const build_path: []const u8 = try b.cache_root.join(b.allocator, &.{ &rel_build_path_base, field.name });

            { // configure
                argv_buf.clearRetainingCapacity();

                try argv_buf.append(self.cmake_exe);
                try argv_buf.appendSlice(&.{ "-S", source_path });
                try argv_buf.appendSlice(&.{ "-B", build_path });
                try argv_buf.appendSlice(&.{ "-G", "Ninja" });

                try argv_buf.append(b.fmt("-DCMAKE_INSTALL_PREFIX={s}", .{install_dir_path}));
                try argv_buf.append(b.fmt("-DCMAKE_PREFIX_PATH={s}", .{install_dir_path}));

                try argv_buf.append("-DBUILD_TESTING=" ++ "OFF");

                switch (dep_id) {
                    .lc => {
                        try argv_buf.append("-DDISABLE_GO=" ++ "ON");
                        try argv_buf.append("-DDISABLE_PERL=" ++ "ON");
                        try argv_buf.append("-DBUILD_TOOL=" ++ "OFF"); // TODO: I assume this disables outputting the executables
                    },
                    .s2n_tls => {
                        try argv_buf.append("-DSEARCH_LIBCRYPTO=" ++ "OFF"); // TODO: no idea what this does but doesn't break anything
                    },

                    .c_common => {},
                    .checksums => {},
                    .c_cal => {},
                    .c_io => {},
                    .c_compression => {},
                    .c_http => {},
                    .c_sdkutils => {},
                    .c_auth => {},
                    .c_s3 => {},
                }

                try Build.Step.handleVerbose(b, null, argv_buf.items);
                var config_cp = std.ChildProcess.init(argv_buf.items, b.allocator);
                try Build.Step.handleChildProcessTerm(step, try config_cp.spawnAndWait(), null, argv_buf.items);
            }

            { // install
                argv_buf.clearRetainingCapacity();

                try argv_buf.append(self.cmake_exe);
                try argv_buf.appendSlice(&.{ "--build", build_path });
                try argv_buf.appendSlice(&.{ "--target", "install" });

                try Build.Step.handleVerbose(b, null, argv_buf.items);
                var install_cp = std.ChildProcess.init(argv_buf.items, b.allocator);
                try Build.Step.handleChildProcessTerm(step, try install_cp.spawnAndWait(), null, argv_buf.items);
            }
        }

        try man.writeManifest();
    }
};

fn relativeLazyPath(b: *Build, lp: Build.LazyPath, rel: []const u8) Build.LazyPath {
    const RelativeLazyPath = struct {
        step: Build.Step,
        lp: Build.LazyPath,
        rel: []const u8,
        generated: Build.GeneratedFile,

        pub const id: Build.Step.Id = .custom;

        fn make(step: *Build.Step, prog: *std.Progress.Node) anyerror!void {
            _ = prog;
            const rlp_step = @fieldParentPtr(@This(), "step", step);
            const base_path = rlp_step.lp.getPath2(step.owner, step);
            rlp_step.generated.path = step.owner.pathJoin(&.{ base_path, rlp_step.rel });
        }
    };
    const rlp_step = b.allocator.create(RelativeLazyPath) catch |e| @panic(@errorName(e));
    rlp_step.* = .{
        .step = Build.Step.init(.{
            .id = RelativeLazyPath.id,
            .name = b.fmt("Relative path '{s}'", .{rel}),
            .owner = b,
            .makeFn = RelativeLazyPath.make,
        }),
        .lp = lp,
        .rel = b.dupe(rel),
        .generated = .{ .step = &rlp_step.step },
    };
    rlp_step.lp.addStepDependencies(&rlp_step.step);
    return .{ .generated = &rlp_step.generated };
}

fn failureStep(
    b: *Build,
    err: anyerror,
    msg: []const u8,
) *Build.Step {
    const log = std.log.default;
    const msg_duped = b.dupe(msg);

    const FailStep = struct {
        step: Build.Step,
        msg: []const u8,
        err: anyerror,

        fn make(step: *Build.Step, _: *std.Progress.Node) anyerror!void {
            const failure = @fieldParentPtr(@This(), "step", step);
            log.err("{s}", .{failure.msg});
            return failure.err;
        }
    };

    const failure: *FailStep = b.allocator.create(FailStep) catch |e| @panic(@errorName(e));
    failure.* = .{
        .step = Build.Step.init(.{
            .id = .custom,
            .name = b.fmt("Failure '{s}'", .{@errorName(err)}),
            .owner = b,
            .makeFn = FailStep.make,
        }),
        .msg = msg_duped,
        .err = err,
    };

    return &failure.step;
}
