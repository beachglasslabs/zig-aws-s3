const std = @import("std");
const Build = std.Build;

const CmakeOptimize = @import("src/shared.zig").CmakeOptimize;
const DepName = @import("src/shared.zig").DepName;

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

    const cmake_cmds_exe = b.addExecutable(.{
        .name = "cmake-cmds",
        .root_source_file = Build.LazyPath.relative("src/cmake-cmds.zig"),
        .optimize = .Debug,
    });
    const cmake_cmds_run = b.addRunArtifact(cmake_cmds_exe);
    cmake_cmds_run.addArgs(&.{
        "--os-tag",   @tagName(target.getOsTag()),
        "--verbose",  if (b.verbose) "true" else "false",
        "--optimize", @tagName(CmakeOptimize.from(optimize)),
    });
    if (cmake_exe_path) |exe_path| cmake_cmds_run.addArgs(&.{ "--cmake-exe", exe_path });

    _ = cmake_cmds_run.addPrefixedOutputFileArg("--build-dir=", "build");
    const aws_cmake_install_dir = cmake_cmds_run.addPrefixedOutputFileArg("--install-dir=", "prefix");

    // NOTE: alright so originally we were trying to use the depfile feature,
    // but that seems to not work as expected, so we're just going to rely
    // on the paths to the dependencies being hashes of the dependency contents
    // to invalidate the cache when they change.

    // zig fmt: off
    cmake_cmds_run.addPrefixedDirectoryArg("--dep-" ++ @tagName(DepName.@"aws-lc"           ) ++ "=", lc_dep.path(""));
    cmake_cmds_run.addPrefixedDirectoryArg("--dep-" ++ @tagName(DepName.@"s2n-tls"          ) ++ "=", s2n_tls_dep.path(""));

    cmake_cmds_run.addPrefixedDirectoryArg("--dep-" ++ @tagName(DepName.@"aws-c-common"     ) ++ "=", c_common_dep.path(""));
    cmake_cmds_run.addPrefixedDirectoryArg("--dep-" ++ @tagName(DepName.@"aws-checksums"    ) ++ "=", checksums_dep.path(""));
    cmake_cmds_run.addPrefixedDirectoryArg("--dep-" ++ @tagName(DepName.@"aws-c-cal"        ) ++ "=", c_cal_dep.path(""));
    cmake_cmds_run.addPrefixedDirectoryArg("--dep-" ++ @tagName(DepName.@"aws-c-io"         ) ++ "=", c_io_dep.path(""));
    cmake_cmds_run.addPrefixedDirectoryArg("--dep-" ++ @tagName(DepName.@"aws-c-compression") ++ "=", c_compression_dep.path(""));
    cmake_cmds_run.addPrefixedDirectoryArg("--dep-" ++ @tagName(DepName.@"aws-c-http"       ) ++ "=", c_http_dep.path(""));
    cmake_cmds_run.addPrefixedDirectoryArg("--dep-" ++ @tagName(DepName.@"aws-c-sdkutils"   ) ++ "=", c_sdkutils_dep.path(""));
    cmake_cmds_run.addPrefixedDirectoryArg("--dep-" ++ @tagName(DepName.@"aws-c-auth"       ) ++ "=", c_auth_dep.path(""));

    cmake_cmds_run.addPrefixedDirectoryArg("--dep-" ++ @tagName(DepName.@"aws-c-s3"         ) ++ "=", c_s3_dep.path(""));
    // zig fmt: on

    _ = b.addModule(module_hack_name, .{ .source_file = aws_cmake_install_dir });
}

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
            rlp_step.generated.path = try std.fs.path.resolve(step.owner.allocator, &.{ base_path, rlp_step.rel });
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
