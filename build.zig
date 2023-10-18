const std = @import("std");
const Build = std.Build;

pub fn build(b: *Build) void {
    // # build options
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const cmake_exe_path = b.option([]const u8, "cmake_exe", "Specifies the CMake executable to invoke for build");

    // # dependencies
    const aws_deps: std.enums.EnumFieldStruct(AwsDepId, *Build.Dependency, null) = .{
        // linux-only
        .lc = b.dependency("awslabs/aws-lc", .{}),
        .s2n_tls = b.dependency("aws/s2n-tls", .{}),

        // remaining
        .c_common = b.dependency("awslabs/aws-c-common", .{}),
        .checksums = b.dependency("awslabs/aws-checksums", .{}),
        .c_cal = b.dependency("awslabs/aws-c-cal", .{}),
        .c_io = b.dependency("awslabs/aws-c-io", .{}),
        .c_compression = b.dependency("awslabs/aws-c-compression", .{}),
        .c_http = b.dependency("awslabs/aws-c-http", .{}),
        .c_sdkutils = b.dependency("awslabs/aws-c-sdkutils", .{}),
        .c_auth = b.dependency("awslabs/aws-c-auth", .{}),

        // the main dependency
        .c_s3 = b.dependency("awslabs/aws-c-s3", .{}),
    };

    // # everything else

    const cmake_install_dir: Build.LazyPath = cmake: {
        const exe_path = cmake_exe_path orelse {
            const poison_dir = b.allocator.create(Build.GeneratedFile) catch |e| @panic(@errorName(e));
            poison_dir.* = .{ .step = failureStep(b, error.CmakeExePathUnspecified, "Must specify cmake_exe to use CMake build") };
            break :cmake .{ .generated = poison_dir };
        };
        const opt = CmakeOptimize.from(optimize);

        const prev: ?*CmakeBuildStep = switch (target.getOsTag()) {
            .linux => blk: {
                // zig fmt: off
                const lc      = cmakeBuildStep(b, "lc",      exe_path, opt, aws_deps.lc.path(""),      null);
                const s2n_tls = cmakeBuildStep(b, "s2n_tls", exe_path, opt, aws_deps.s2n_tls.path(""), lc.installPrefix());
                // zig fmt: on
                break :blk s2n_tls;
            },
            else => null,
        };

        // zig fmt: off
        const c_common      = cmakeBuildStep(b, "c_common",      exe_path, opt, aws_deps.c_common.path(""),      if (prev) |cbs| cbs.installPrefix() else null);
        const checksums     = cmakeBuildStep(b, "checksums",     exe_path, opt, aws_deps.checksums.path(""),     c_common.installPrefix());
        const c_cal         = cmakeBuildStep(b, "c_cal",         exe_path, opt, aws_deps.c_cal.path(""),         checksums.installPrefix());
        const c_io          = cmakeBuildStep(b, "c_io",          exe_path, opt, aws_deps.c_io.path(""),          c_cal.installPrefix());
        const c_compression = cmakeBuildStep(b, "c_compression", exe_path, opt, aws_deps.c_compression.path(""), c_io.installPrefix());
        const c_http        = cmakeBuildStep(b, "c_http",        exe_path, opt, aws_deps.c_http.path(""),        c_compression.installPrefix());
        const c_sdkutils    = cmakeBuildStep(b, "c_sdkutils",    exe_path, opt, aws_deps.c_sdkutils.path(""),    c_http.installPrefix());
        const c_auth        = cmakeBuildStep(b, "c_auth",        exe_path, opt, aws_deps.c_auth.path(""),        c_sdkutils.installPrefix());

        const c_s3          = cmakeBuildStep(b, "c_s3",          exe_path, opt, aws_deps.c_s3.path(""),          c_auth.installPrefix());
        // zig fmt: on

        break :cmake c_s3.installPrefix();
    };

    const proxy_artifact = b.addStaticLibrary(.{
        .name = "lib",
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(proxy_artifact);

    proxy_artifact.installHeadersDirectoryOptions(.{
        .source_dir = relativeLazyPath(b, cmake_install_dir, "include"),
        .install_dir = .header,
        .install_subdir = "",
    });
    proxy_artifact.addLibraryPath(relativeLazyPath(b, cmake_install_dir, "lib"));
    for (&[_][]const u8{
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
    }) |libname| proxy_artifact.linkSystemLibrary2(libname, .{ .needed = true, .use_pkg_config = .no, .preferred_link_mode = .Static });
}

const AwsDepId = enum {
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

const CmakeOptimize = enum {
    Debug,
    MinSizeRel,
    Release,
    RelWithDebInfo,

    inline fn from(optimize: std.builtin.Mode) CmakeOptimize {
        return switch (optimize) {
            .Debug => .Debug,
            .ReleaseSmall => .MinSizeRel,
            .ReleaseFast => .Release,
            .ReleaseSafe => .RelWithDebInfo,
        };
    }
};

fn cmakeBuildStep(
    b: *Build,
    name: []const u8,
    cmake_exe: []const u8,
    optimize: CmakeOptimize,
    source: Build.LazyPath,
    prefix: ?Build.LazyPath,
) *CmakeBuildStep {
    return CmakeBuildStep.create(.{
        .builder = b,
        .name = name,
        .cmake_exe = cmake_exe,
        .optimize = optimize,
        .source = source,
        .prefix = prefix,
    });
}

const CmakeBuildStep = struct {
    step: Build.Step,
    cmake_exe: []const u8,
    source: Build.LazyPath,
    optimize: CmakeOptimize,
    existing_prefix: ?Build.LazyPath,
    /// If `existing_prefix` is non-null, this field
    /// mirrors it, but with this step as an intermediate
    /// dependency instead.
    generated_prefix: Build.GeneratedFile,

    pub const id: Build.Step.Id = .custom;

    pub const CreateOptions = struct {
        builder: *Build,
        name: []const u8,
        /// Path to the cmake executable to be invoked.
        cmake_exe: []const u8,
        /// The `-DCMAKE_BUILD_TYPE` value.
        optimize: CmakeOptimize,
        /// The source directory that will be passed to `-S`.
        source: Build.LazyPath,
        /// If null, a new install prefix will be generated.
        /// If non-null, will be used as the install prefix.
        /// When this step is dependent on another `CmakeconfigAndInstallStep`,
        /// this should be the `installPrefix()` of said step.
        prefix: ?Build.LazyPath,
    };

    pub fn create(options: CreateOptions) *CmakeBuildStep {
        const b = options.builder;
        const self = b.allocator.create(CmakeBuildStep) catch |e| @panic(@errorName(e));
        self.* = .{
            .step = Build.Step.init(.{
                .id = CmakeBuildStep.id,
                .name = b.fmt("CMake ({s})", .{options.name}),
                .owner = b,
                .makeFn = CmakeBuildStep.make,
            }),
            .cmake_exe = b.dupe(options.cmake_exe),
            .source = options.source,
            .optimize = options.optimize,
            .existing_prefix = options.prefix,
            .generated_prefix = .{ .step = &self.step },
        };
        self.source.addStepDependencies(&self.step);
        if (self.existing_prefix) |prefix| {
            prefix.addStepDependencies(&self.step);
        }
        return self;
    }

    pub fn installPrefix(self: *CmakeBuildStep) Build.LazyPath {
        return .{ .generated = &self.generated_prefix };
    }

    fn make(step: *Build.Step, prog: *std.Progress.Node) anyerror!void {
        _ = prog;

        const self = @fieldParentPtr(CmakeBuildStep, "step", step);
        const b = step.owner;

        const source_path = self.source.getPath2(b, step);

        var man = b.cache.obtain();
        defer man.deinit();

        man.hash.addBytes(self.cmake_exe);
        man.hash.addBytes(std.mem.asBytes(&self.optimize));

        {
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

                if (dirname != null and
                    !std.mem.startsWith(u8, dirname.?, "source") and
                    !std.mem.startsWith(u8, dirname.?, "include") //
                ) continue;
                if (!std.mem.eql(u8, ext, ".c") and
                    !std.mem.eql(u8, basename, "CMakeLists.txt") //
                ) continue;

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
        const rel_build_path = rel_cache_path ++ std.fs.path.sep_str.* ++ "build".*;

        const build_path = try b.cache_root.join(b.allocator, &.{&rel_build_path});
        const install_path = if (self.existing_prefix) |existing|
            existing.getPath2(b, step)
        else blk: {
            const install_path = rel_cache_path ++ std.fs.path.sep_str.* ++ "install".*;
            try b.cache_root.handle.makePath(&install_path);
            break :blk try b.cache_root.join(b.allocator, &.{&install_path});
        };

        self.generated_prefix.path = install_path;
        if (hit) return;

        var argv = try std.ArrayList([]const u8).initCapacity(b.allocator, 32);
        defer argv.deinit();

        { // configure
            argv.clearRetainingCapacity();
            try argv.append(self.cmake_exe);
            try argv.appendSlice(&.{ "-S", source_path });
            try argv.appendSlice(&.{ "-B", build_path });
            try argv.append(b.fmt("-DCMAKE_INSTALL_PREFIX={s}", .{install_path}));
            try argv.append(b.fmt("-DCMAKE_PREFIX_PATH={s}", .{install_path}));

            var config_cp = std.ChildProcess.init(argv.items, b.allocator);
            config_cp.stdout_behavior = .Inherit;
            config_cp.stderr_behavior = .Inherit;
            try switch (try config_cp.spawnAndWait()) {
                .Exited => |exit_code| if (exit_code != 0) error.CmakeConfigNonZeroExit,
                .Signal => error.CmakeConfigSignalled,
                .Stopped => error.CmakeConfigStopped,
                .Unknown => error.CmakeConfigHaltedForUnknownReason,
            };
        }

        { // install
            argv.clearRetainingCapacity();
            try argv.append(self.cmake_exe);
            try argv.appendSlice(&.{ "--build", build_path });
            try argv.appendSlice(&.{ "--target", "install" });

            var install_cp = std.ChildProcess.init(argv.items, b.allocator);
            install_cp.stdout_behavior = .Inherit;
            install_cp.stderr_behavior = .Inherit;
            try switch (try install_cp.spawnAndWait()) {
                .Exited => |exit_code| if (exit_code != 0) error.CmakeInstallNonZeroExit,
                .Signal => error.CmakeInstallSignalled,
                .Stopped => error.CmakeInstallStopped,
                .Unknown => error.CmakeInstallHaltedForUnknownReason,
            };
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
