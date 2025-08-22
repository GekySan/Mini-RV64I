const std = @import("std");
const builtin = @import("builtin");

pub fn build(b: *std.Build) void {
    // Vérification de la version du compilateur
    const required_version_str = "0.14.1";
    const required_version = std.SemanticVersion.parse(required_version_str) catch |err| {
        std.debug.panic(
            "La chaîne de version requise '{s}' est invalide: {s}",
            .{ required_version_str, @errorName(err) },
        );
    };

    const current_version = builtin.zig_version;
    if (current_version.order(required_version) != .eq) {
        std.debug.panic(
            \\Ce projet doit être compilé exactement avec Zig {any}.
            \\Version actuellement utilisée : {any}
        ,
            .{ required_version, current_version },
        );
    }

    // Build de l'exécutable
    const exe = b.addExecutable(.{
        .name = "test",
        .root_source_file = b.path("core.zig"),
        .target = b.standardTargetOptions(.{}), // << requis
        .optimize = b.standardOptimizeOption(.{}), // << recommandé
    });

    b.installArtifact(exe);
}
