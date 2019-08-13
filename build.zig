const Builder = @import("std").build.Builder;
const builtin = @import("builtin");

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();
    const windows = b.option(
        bool,
        "windows",
        "create windows build",
    ) orelse false;

    const exe = b.addExecutable("as404", "src/main.zig");
    exe.setBuildMode(mode);
    if (windows) {
        exe.setTarget(
            builtin.Arch.x86_64,
            builtin.Os.windows,
            builtin.Abi.gnu,
        );
    }
    exe.linkSystemLibrary("c");
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
