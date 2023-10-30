const std = @import("std");

pub const CmakeOptimize = enum {
    Debug,
    MinSizeRel,
    Release,
    RelWithDebInfo,

    pub inline fn from(optimize: std.builtin.Mode) CmakeOptimize {
        return switch (optimize) {
            .Debug => .Debug,
            .ReleaseSmall => .MinSizeRel,
            .ReleaseFast => .Release,
            .ReleaseSafe => .RelWithDebInfo,
        };
    }
};

pub const DepName = enum {
    //! Dependency names declared in hierarchical order; that is
    //! to say, each dependency save the first depends on at least
    //! one dependency which appears before it, and on none which
    //! come after it.

    // indirect dependencies only required on linux
    @"aws-lc",
    @"s2n-tls",

    // indirect dependencies which are always required
    @"aws-c-common",
    @"aws-checksums",
    @"aws-c-cal",
    @"aws-c-io",
    @"aws-c-compression",
    @"aws-c-http",
    @"aws-c-sdkutils",
    @"aws-c-auth",

    // direct dependencies which are always required
    @"aws-c-s3",

    pub inline fn neededFor(dn: DepName, os_tag: std.Target.Os.Tag) bool {
        return switch (dn) {
            .@"aws-lc",
            .@"s2n-tls",
            => switch (os_tag) {
                .linux => true,
                else => false,
            },

            .@"aws-c-common",
            .@"aws-checksums",
            .@"aws-c-cal",
            .@"aws-c-io",
            .@"aws-c-compression",
            .@"aws-c-http",
            .@"aws-c-sdkutils",
            .@"aws-c-auth",

            .@"aws-c-s3",
            => true,
        };
    }
};
