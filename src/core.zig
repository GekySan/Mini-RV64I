/// À compiler avec Zig 0.14.1
const std = @import("std");

/// Constantes globales
const mem_size: usize = 1 << 20; // 1 MiB
const Ux = u64; // Unsigned XLEN (RV64)
const Sx = i64; // Signed   XLEN (RV64)
const reset_pc: Ux = 0;

/// OpCodes
const OP_LUI: u32 = 0x37;
const OP_AUIPC: u32 = 0x17;
const OP_JAL: u32 = 0x6F;
const OP_JALR: u32 = 0x67;
const OP_BRANCH: u32 = 0x63;
const OP_LOAD: u32 = 0x03;
const OP_STORE: u32 = 0x23;
const OP_OPIMM: u32 = 0x13;
const OP_OPIMM32: u32 = 0x1B;
const OP_OP: u32 = 0x33;
const OP_OP32: u32 = 0x3B;
const OP_MISC: u32 = 0x0F; // FENCE / FENCE.I
const OP_SYSTEM: u32 = 0x73;

/// CPU & Traps
const Trap = enum(u8) {
    none = 0,
    fetch,
    illegal,
    load,
    store,
    ecall,
    ebreak,
};

const Cpu = struct {
    pc: Ux,
    x: [32]Ux,
    mem: [mem_size]u8,
    cycles: u64,
    halted: bool,
    trap: Trap,
};

/// Utilitaires d'extraction
inline fn opcode(i: u32) u32 {
    return i & 0x7f;
}
inline fn rd(i: u32) u32 {
    return (i >> 7) & 0x1f;
}
inline fn funct3(i: u32) u32 {
    return (i >> 12) & 0x7;
}
inline fn rs1(i: u32) u32 {
    return (i >> 15) & 0x1f;
}
inline fn rs2(i: u32) u32 {
    return (i >> 20) & 0x1f;
}
inline fn funct7(i: u32) u32 {
    return (i >> 25) & 0x7f;
}

/// Extension de signe sûre (1 <= bits <= 63)
inline fn signExtendU(v: Ux, bits: u6) Ux {
    const m: Ux = (@as(Ux, 1)) << (bits - 1);
    return (v ^ m) -% m;
}

/// Immédiats
inline fn immI(insn: u32) Sx {
    const u: Ux = (@as(Ux, @intCast(insn >> 20))) & 0x0fff;
    return @as(Sx, @bitCast(signExtendU(u, 12)));
}
inline fn immS(insn: u32) Sx {
    var u: Ux = ((@as(Ux, @intCast(insn >> 7))) & 0x1f) | ((@as(Ux, @intCast(insn >> 25))) << 5);
    u &= 0x0fff;
    return @as(Sx, @bitCast(signExtendU(u, 12)));
}
inline fn immB(insn: u32) Sx {
    var u: Ux = 0;
    u |= ((@as(Ux, @intCast(insn >> 8))) & 0x0f) << 1; // [11:8]  -> [4:1]
    u |= ((@as(Ux, @intCast(insn >> 25))) & 0x3f) << 5; // [30:25] -> [10:5]
    u |= ((@as(Ux, @intCast(insn >> 7))) & 0x01) << 11; // [7]     -> [11]
    u |= if ((insn & 0x8000_0000) != 0) 0x1000 else 0; // bit 12
    return @as(Sx, @bitCast(signExtendU(u, 13)));
}
inline fn immJ(insn: u32) Sx {
    var u: Ux = 0;
    u |= ((@as(Ux, @intCast(insn >> 21))) & 0x3ff) << 1; // [30:21] -> [10:1]
    u |= ((@as(Ux, @intCast(insn >> 20))) & 0x01) << 11; // [20]    -> [11]
    u |= ((@as(Ux, @intCast(insn >> 12))) & 0xff) << 12; // [19:12] -> [19:12]
    u |= if ((insn & 0x8000_0000) != 0) 0x100000 else 0; // bit 20
    return @as(Sx, @bitCast(signExtendU(u, 21)));
}
inline fn immU(insn: u32) Sx {
    return @as(Sx, @intCast((@as(Ux, @intCast(insn)) & 0xffff_f000)));
}

/// Mémoire
inline fn inRange(a: Ux, n: usize) bool {
    return a <= (@as(Ux, @intCast(mem_size)) - @as(Ux, @intCast(n)));
}

fn load8(c: *Cpu, a: Ux) !u8 {
    if (!inRange(a, 1)) return error.OutOfRange;
    return c.mem[@as(usize, @intCast(a))];
}
fn load16(c: *Cpu, a: Ux) !u16 {
    if (!inRange(a, 2)) return error.OutOfRange;
    const i = @as(usize, @intCast(a));
    return (@as(u16, c.mem[i])) | (@as(u16, c.mem[i + 1]) << 8);
}
fn load32(c: *Cpu, a: Ux) !u32 {
    if (!inRange(a, 4)) return error.OutOfRange;
    const i = @as(usize, @intCast(a));
    return (@as(u32, c.mem[i])) | (@as(u32, c.mem[i + 1]) << 8) | (@as(u32, c.mem[i + 2]) << 16) | (@as(u32, c.mem[i + 3]) << 24);
}
fn load64(c: *Cpu, a: Ux) !u64 {
    if (!inRange(a, 8)) return error.OutOfRange;
    const i = @as(usize, @intCast(a));
    return (@as(u64, c.mem[i])) | (@as(u64, c.mem[i + 1]) << 8) | (@as(u64, c.mem[i + 2]) << 16) | (@as(u64, c.mem[i + 3]) << 24) | (@as(u64, c.mem[i + 4]) << 32) | (@as(u64, c.mem[i + 5]) << 40) | (@as(u64, c.mem[i + 6]) << 48) | (@as(u64, c.mem[i + 7]) << 56);
}

fn store8(c: *Cpu, a: Ux, v: u8) !void {
    if (!inRange(a, 1)) return error.OutOfRange;
    c.mem[@as(usize, @intCast(a))] = v;
}
fn store16(c: *Cpu, a: Ux, v: u16) !void {
    if (!inRange(a, 2)) return error.OutOfRange;
    const i = @as(usize, @intCast(a));
    c.mem[i + 0] = @as(u8, @truncate(v));
    c.mem[i + 1] = @as(u8, @truncate(v >> 8));
}
fn store32(c: *Cpu, a: Ux, v: u32) !void {
    if (!inRange(a, 4)) return error.OutOfRange;
    const i = @as(usize, @intCast(a));
    c.mem[i + 0] = @as(u8, @truncate(v));
    c.mem[i + 1] = @as(u8, @truncate(v >> 8));
    c.mem[i + 2] = @as(u8, @truncate(v >> 16));
    c.mem[i + 3] = @as(u8, @truncate(v >> 24));
}
fn store64(c: *Cpu, a: Ux, v: u64) !void {
    if (!inRange(a, 8)) return error.OutOfRange;
    const i = @as(usize, @intCast(a));
    c.mem[i + 0] = @as(u8, @truncate(v));
    c.mem[i + 1] = @as(u8, @truncate(v >> 8));
    c.mem[i + 2] = @as(u8, @truncate(v >> 16));
    c.mem[i + 3] = @as(u8, @truncate(v >> 24));
    c.mem[i + 4] = @as(u8, @truncate(v >> 32));
    c.mem[i + 5] = @as(u8, @truncate(v >> 40));
    c.mem[i + 6] = @as(u8, @truncate(v >> 48));
    c.mem[i + 7] = @as(u8, @truncate(v >> 56));
}

/// Helpers: sign-extend vers Ux (LB/LH/LW)
inline fn signExtend8ToUx(v: u8) Ux {
    const s8: i8 = @as(i8, @bitCast(v));
    const s64: Sx = @as(Sx, @intCast(s8));
    return @as(Ux, @bitCast(s64));
}
inline fn signExtend16ToUx(v: u16) Ux {
    const s16: i16 = @as(i16, @bitCast(v));
    const s64: Sx = @as(Sx, @intCast(s16));
    return @as(Ux, @bitCast(s64));
}
inline fn signExtend32ToUx(v: u32) Ux {
    const s32: i32 = @as(i32, @bitCast(v));
    const s64: Sx = @as(Sx, @intCast(s32));
    return @as(Ux, @bitCast(s64));
}

/// Registres & débogue
inline fn readReg(c: *const Cpu, r: u32) Ux {
    return if (r != 0) c.x[@as(usize, @intCast(r))] else 0;
}
inline fn writeReg(c: *Cpu, r: u32, v: Ux) void {
    if (r != 0) c.x[@as(usize, @intCast(r))] = v;
}

fn dump(c: *const Cpu) void {
    const n = [_][]const u8{
        "x0 ", "ra ", "sp ", "gp ", "tp ", "t0 ", "t1 ", "t2 ",
        "s0 ", "s1 ", "a0 ", "a1 ", "a2 ", "a3 ", "a4 ", "a5 ",
        "a6 ", "a7 ", "s2 ", "s3 ", "s4 ", "s5 ", "s6 ", "s7 ",
        "s8 ", "s9 ", "s10", "s11", "t3 ", "t4 ", "t5 ", "t6 ",
    };
    std.debug.print("pc=0x{x:0>16}  cycles={d}\n", .{ c.pc, c.cycles });
    var i: usize = 0;
    while (i < 32) : (i += 1) {
        const sep: u8 = if ((i % 4) == 3) '\n' else ' ';
        std.debug.print("{s}=0x{x:0>16}{c}", .{ n[i], c.x[i], sep });
    }
}

/// Centralisation des erreurs/halts
fn trap(c: *Cpu, t: Trap, addr: Ux, msg: ?[]const u8) void {
    if (msg) |m| {
        if (addr != 0 or t == .fetch or t == .load or t == .store)
            std.debug.print("{s} @0x{x:0>16}\n", .{ m, addr })
        else
            std.debug.print("{s}\n", .{m});
    }
    c.trap = t;
    c.halted = true;
}

/// Exécuter une instruction
fn step(c: *Cpu) void {
    // Alignement 32-bit exigé (RV64I sans C)
    if ((c.pc & 3) != 0) {
        trap(c, .fetch, c.pc, "Misaligned fetch");
        return;
    }

    const insn = load32(c, c.pc) catch {
        trap(c, .fetch, c.pc, "Fetch fault");
        return;
    };

    var pc_next: Ux = c.pc +% 4;

    switch (opcode(insn)) {
        // U-type
        OP_LUI => {
            writeReg(c, rd(insn), @as(Ux, @intCast(immU(insn))));
        },
        OP_AUIPC => {
            writeReg(c, rd(insn), c.pc +% @as(Ux, @intCast(immU(insn))));
        },

        // Jumps
        OP_JAL => {
            const off = immJ(insn);
            writeReg(c, rd(insn), pc_next);
            pc_next = c.pc +% @as(Ux, @bitCast(off));
        },
        OP_JALR => {
            const off = immI(insn);
            const t = readReg(c, rs1(insn)) +% @as(Ux, @bitCast(off));
            writeReg(c, rd(insn), pc_next);
            pc_next = t & ~@as(Ux, 1);
        },

        // Branches
        OP_BRANCH => {
            const off = immB(insn);
            const a = readReg(c, rs1(insn));
            const b = readReg(c, rs2(insn));
            var take = false;
            switch (funct3(insn)) {
                0 => take = (a == b), // BEQ
                1 => take = (a != b), // BNE
                4 => take = (@as(Sx, @bitCast(a)) < @as(Sx, @bitCast(b))), // BLT
                5 => take = (@as(Sx, @bitCast(a)) >= @as(Sx, @bitCast(b))), // BGE
                6 => take = (a < b), // BLTU
                7 => take = (a >= b), // BGEU
                else => {
                    trap(c, .illegal, 0, "Illegal BR");
                    return;
                },
            }
            if (take) pc_next = c.pc +% @as(Ux, @bitCast(off));
        },

        // Loads
        OP_LOAD => {
            const off = immI(insn);
            const ea = readReg(c, rs1(insn)) +% @as(Ux, @bitCast(off));
            switch (funct3(insn)) {
                0 => { // LB
                    const v = load8(c, ea) catch {
                        trap(c, .load, ea, "Load fault");
                        return;
                    };
                    writeReg(c, rd(insn), signExtend8ToUx(v));
                },
                1 => { // LH
                    const v = load16(c, ea) catch {
                        trap(c, .load, ea, "Load fault");
                        return;
                    };
                    writeReg(c, rd(insn), signExtend16ToUx(v));
                },
                2 => { // LW
                    const v = load32(c, ea) catch {
                        trap(c, .load, ea, "Load fault");
                        return;
                    };
                    writeReg(c, rd(insn), signExtend32ToUx(v));
                },
                3 => { // LD
                    const v = load64(c, ea) catch {
                        trap(c, .load, ea, "Load fault");
                        return;
                    };
                    writeReg(c, rd(insn), v);
                },
                4 => { // LBU
                    const v = load8(c, ea) catch {
                        trap(c, .load, ea, "Load fault");
                        return;
                    };
                    writeReg(c, rd(insn), @as(Ux, @intCast(v)));
                },
                5 => { // LHU
                    const v = load16(c, ea) catch {
                        trap(c, .load, ea, "Load fault");
                        return;
                    };
                    writeReg(c, rd(insn), @as(Ux, @intCast(v)));
                },
                6 => { // LWU
                    const v = load32(c, ea) catch {
                        trap(c, .load, ea, "Load fault");
                        return;
                    };
                    writeReg(c, rd(insn), @as(Ux, @intCast(v)));
                },
                else => {
                    trap(c, .illegal, 0, "Illegal LOAD");
                    return;
                },
            }
        },

        // Stores
        OP_STORE => {
            const off = immS(insn);
            const ea = readReg(c, rs1(insn)) +% @as(Ux, @bitCast(off));
            const v = readReg(c, rs2(insn));
            switch (funct3(insn)) {
                0 => store8(c, ea, @as(u8, @intCast(v))) catch {
                    trap(c, .store, ea, "Store fault");
                    return;
                }, // SB
                1 => store16(c, ea, @as(u16, @intCast(v))) catch {
                    trap(c, .store, ea, "Store fault");
                    return;
                }, // SH
                2 => store32(c, ea, @as(u32, @intCast(v))) catch {
                    trap(c, .store, ea, "Store fault");
                    return;
                }, // SW
                3 => store64(c, ea, v) catch {
                    trap(c, .store, ea, "Store fault");
                    return;
                }, // SD
                else => {
                    trap(c, .illegal, 0, "Illegal STORE");
                    return;
                },
            }
        },

        // OP-IMM
        OP_OPIMM => {
            const imm = immI(insn);
            const a = readReg(c, rs1(insn));
            switch (funct3(insn)) {
                0x0 => writeReg(c, rd(insn), a +% @as(Ux, @bitCast(imm))), // ADDI
                0x2 => writeReg(c, rd(insn), if (@as(Sx, @bitCast(a)) < imm) 1 else 0), // SLTI
                0x3 => writeReg(c, rd(insn), if (a < @as(Ux, @bitCast(imm))) 1 else 0), // SLTIU
                0x4 => writeReg(c, rd(insn), a ^ @as(Ux, @bitCast(imm))), // XORI
                0x6 => writeReg(c, rd(insn), a | @as(Ux, @bitCast(imm))), // ORI
                0x7 => writeReg(c, rd(insn), a & @as(Ux, @bitCast(imm))), // ANDI
                0x1 => { // SLLI
                    const sh: u6 = @as(u6, @intCast((insn >> 20) & 0x3f));
                    writeReg(c, rd(insn), a << sh);
                },
                0x5 => { // SRLI / SRAI
                    const sh: u6 = @as(u6, @intCast((insn >> 20) & 0x3f));
                    if (((insn >> 30) & 1) != 0)
                        writeReg(c, rd(insn), @as(Ux, @bitCast(@as(Sx, @bitCast(a)) >> sh))) // SRAI
                    else
                        writeReg(c, rd(insn), a >> sh); // SRLI
                },
                else => {
                    trap(c, .illegal, 0, "Illegal OP-IMM");
                    return;
                },
            }
        },

        // OP-IMM-32 (RV64)
        OP_OPIMM32 => {
            const imm = immI(insn);
            const a32: u32 = @as(u32, @truncate(readReg(c, rs1(insn))));
            var res64: Sx = 0;
            switch (funct3(insn)) {
                0x0 => { // ADDIW
                    const r32: i32 = @as(i32, @bitCast(a32)) +% @as(i32, @intCast(imm));
                    res64 = @as(Sx, r32);
                },
                0x1 => { // SLLIW
                    const sh: u5 = @as(u5, @intCast((insn >> 20) & 0x1f));
                    const r32: u32 = a32 << sh;
                    res64 = @as(Sx, @as(i32, @bitCast(r32)));
                },
                0x5 => { // SRLIW / SRAIW
                    const sh: u5 = @as(u5, @intCast((insn >> 20) & 0x1f));
                    if (((insn >> 30) & 1) != 0) {
                        const a_s: i32 = @as(i32, @bitCast(a32));
                        res64 = @as(Sx, a_s >> sh);
                    } else {
                        const r32: u32 = a32 >> sh;
                        res64 = @as(Sx, @as(i32, @bitCast(r32)));
                    }
                },
                else => {
                    trap(c, .illegal, 0, "Illegal OP-IMM-32");
                    return;
                },
            }
            writeReg(c, rd(insn), @as(Ux, @bitCast(res64)));
        },

        // OP
        OP_OP => {
            const a = readReg(c, rs1(insn));
            const b = readReg(c, rs2(insn));
            switch (funct3(insn)) {
                0x0 => {
                    if (funct7(insn) == 0x00) writeReg(c, rd(insn), a +% b) // ADD
                    else if (funct7(insn) == 0x20) writeReg(c, rd(insn), a -% b) // SUB
                    else {
                        trap(c, .illegal, 0, "Illegal OP");
                        return;
                    }
                },
                0x1 => { // SLL
                    const sh: u6 = @as(u6, @truncate(b & 63));
                    writeReg(c, rd(insn), a << sh);
                },
                0x2 => writeReg(c, rd(insn), if (@as(Sx, @bitCast(a)) < @as(Sx, @bitCast(b))) 1 else 0), // SLT
                0x3 => writeReg(c, rd(insn), if (a < b) 1 else 0), // SLTU
                0x4 => writeReg(c, rd(insn), a ^ b), // XOR
                0x5 => {
                    const sh: u6 = @as(u6, @truncate(b & 63));
                    if (funct7(insn) == 0x00)
                        writeReg(c, rd(insn), a >> sh) // SRL
                    else if (funct7(insn) == 0x20)
                        writeReg(c, rd(insn), @as(Ux, @bitCast(@as(Sx, @bitCast(a)) >> sh))) // SRA
                    else {
                        trap(c, .illegal, 0, "Illegal OP");
                        return;
                    }
                },
                0x6 => writeReg(c, rd(insn), a | b), // OR
                0x7 => writeReg(c, rd(insn), a & b), // AND
                else => {
                    trap(c, .illegal, 0, "Illegal OP");
                    return;
                },
            }
        },

        // OP-32
        OP_OP32 => {
            const a: u32 = @as(u32, @truncate(readReg(c, rs1(insn))));
            const b: u32 = @as(u32, @truncate(readReg(c, rs2(insn))));
            var res64: Sx = 0;
            switch (funct3(insn)) {
                0x0 => {
                    const a_s: i32 = @as(i32, @bitCast(a));
                    const b_s: i32 = @as(i32, @bitCast(b));
                    if (funct7(insn) == 0x00)
                        res64 = @as(Sx, a_s +% b_s) // ADDW
                    else if (funct7(insn) == 0x20)
                        res64 = @as(Sx, a_s -% b_s) // SUBW
                    else {
                        trap(c, .illegal, 0, "Illegal OP-32");
                        return;
                    }
                },
                0x1 => { // SLLW
                    const sh: u5 = @as(u5, @truncate(b & 31));
                    const r32: u32 = a << sh;
                    res64 = @as(Sx, @as(i32, @bitCast(r32)));
                },
                0x5 => { // SRLW / SRAW
                    const sh: u5 = @as(u5, @truncate(b & 31));
                    if (funct7(insn) == 0x00) {
                        const r32: u32 = a >> sh;
                        res64 = @as(Sx, @as(i32, @bitCast(r32)));
                    } else if (funct7(insn) == 0x20) {
                        const a_s: i32 = @as(i32, @bitCast(a));
                        res64 = @as(Sx, a_s >> sh);
                    } else {
                        trap(c, .illegal, 0, "Illegal OP-32");
                        return;
                    }
                },
                else => {
                    trap(c, .illegal, 0, "Illegal OP-32");
                    return;
                },
            }
            writeReg(c, rd(insn), @as(Ux, @bitCast(res64)));
        },

        // FENCE / FENCE.I -> NOP
        OP_MISC => {},

        // SYSTEM
        OP_SYSTEM => {
            const imm: u32 = insn >> 20;
            if (imm == 0) {
                std.debug.print("[ECALL]\n", .{});
                trap(c, .ecall, 0, null);
                return;
            } else if (imm == 1) {
                std.debug.print("[EBREAK]\n", .{});
                trap(c, .ebreak, 0, null);
                return;
            } else {
                trap(c, .illegal, 0, "CSR non implémenté");
                return;
            }
        },

        else => {
            var buf: [64]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Opcode 0x{X:0>2} non géré", .{opcode(insn)}) catch "Opcode non géré";
            trap(c, .illegal, c.pc, msg);
            return;
        },
    }

    c.x[0] = 0; // x0 reste 0
    c.pc = pc_next;
    c.cycles +%= 1;
}

/// Exemples d'encodeurs
inline fn encAddi(rd_: u32, rs1_: u32, imm_: i32) u32 {
    const u: u32 = @as(u32, @bitCast(imm_)) & 0x0fff;
    return 0x13 | (rd_ << 7) | (0 << 12) | (rs1_ << 15) | (u << 20);
}
inline fn encAdd(rd_: u32, rs1_: u32, rs2_: u32) u32 {
    return 0x33 | (rd_ << 7) | (0 << 12) | (rs1_ << 15) | (rs2_ << 20) | (0x00 << 25);
}
inline fn encAddiw(rd_: u32, rs1_: u32, imm_: i32) u32 {
    const u: u32 = @as(u32, @bitCast(imm_)) & 0x0fff;
    return 0x1B | (rd_ << 7) | (0 << 12) | (rs1_ << 15) | (u << 20);
}
inline fn encSlli(rd_: u32, rs1_: u32, sh_: u32) u32 {
    return 0x13 | (rd_ << 7) | (1 << 12) | (rs1_ << 15) | ((sh_ & 0x3f) << 20);
}
inline fn encJal(rd_: u32, off_: i32) u32 {
    const u = @as(u32, @intCast(off_));
    const b20 = (u >> 20) & 1;
    const b10_1 = (u >> 1) & 0x3ff;
    const b11 = (u >> 11) & 1;
    const b19_12 = (u >> 12) & 0xff;
    return 0x6F | (rd_ << 7) | (b20 << 31) | (b19_12 << 12) | (b11 << 20) | (b10_1 << 21);
}
inline fn encJalr(rd_: u32, rs1_: u32, off_: i32) u32 {
    return 0x67 | (rd_ << 7) | (0 << 12) | (rs1_ << 15) | (((@as(u32, @bitCast(off_)) & 0x0fff)) << 20);
}
inline fn encB(f3v: u32, rs1v: u32, rs2v: u32, off_: i32) u32 {
    const u: u32 = @as(u32, @bitCast(off_));
    const b12 = (u >> 12) & 1;
    const b10_5 = (u >> 5) & 0x3f;
    const b4_1 = (u >> 1) & 0x0f;
    const b11 = (u >> 11) & 1;
    return 0x63 | (b11 << 7) | (f3v << 12) | (rs1v << 15) | (rs2v << 20) | (b10_5 << 25) | (b12 << 31) | (b4_1 << 8);
}
inline fn encLd(rd_: u32, rs1_: u32, off_: i32) u32 {
    const u: u32 = @as(u32, @bitCast(off_)) & 0x0fff;
    return 0x03 | (rd_ << 7) | (0x3 << 12) | (rs1_ << 15) | (u << 20);
}
inline fn encEcall() u32 {
    return 0x0000_0073;
}
inline fn encRet() u32 {
    return encJalr(0, 1, 0);
}

/// Écriture code 32-bit LE via store32 (bornée)
inline fn emit32(c: *Cpu, a: Ux, w: u32) void {
    store32(c, a, w) catch {
        std.debug.print("emit32: hors-mémoire @0x{x:0>16}\n", .{a});
        c.halted = true;
    };
}

/// Programme de test : sum(1..10)
//
// 0x00: addi  a0, x0, 10   ; a0 = 10
// 0x04: jal   ra, +8       ; -> 0x0C
// 0x08: ecall               ; fin après retour
// 0x0C: addi  a1, a0, 0    ; a1 = n
// 0x10: addi  a0, x0, 0    ; a0 = 0 (sum)
// 0x14: add   a0, a0, a1
// 0x18: addi  a1, a1, -1
// 0x1C: bne   a1, x0, 0x14
// 0x20: ret

fn loadDemo(c: *Cpu) void {
    emit32(c, 0x00, encAddi(10, 0, 10));
    emit32(c, 0x04, encJal(1, 8)); // ra=0x08 ; pc=0x0C
    emit32(c, 0x08, encEcall());

    emit32(c, 0x0C, encAddi(11, 10, 0)); // a1=a0
    emit32(c, 0x10, encAddi(10, 0, 0)); // a0=0
    emit32(c, 0x14, encAdd(10, 10, 11)); // sum+=a1
    emit32(c, 0x18, encAddi(11, 11, -1)); // a1--
    emit32(c, 0x1C, encB(0x1, 11, 0, -8)); // BNE a1,x0 -> 0x14
    emit32(c, 0x20, encRet());
}

pub fn main() !void {
    var c = std.mem.zeroes(Cpu);
    c.pc = reset_pc;
    c.x[2] = @as(Ux, @intCast(mem_size)); // sp
    loadDemo(&c);

    std.debug.print("=== RV64I demo : sum(1..10) ===\n", .{});
    dump(&c);
    std.debug.print("\n", .{});

    while (!c.halted and c.cycles < 1000) {
        step(&c);
    }

    std.debug.print("\n=== Fini ===\n", .{});
    dump(&c);
    std.debug.print("\n", .{});

    std.debug.print("a0 (x10) = {d} (attendu 55)\n", .{c.x[10]});
    std.debug.print("{s}\n", .{if (c.x[10] == 55) "SUCCESS" else "FAILURE"});
}
