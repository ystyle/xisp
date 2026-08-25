package ystyle::xisp.core.eval
import std.collection.ArrayList

/**
 * x86-64 机器码发射器（JIT 正式工程 J1）
 * 只依赖 caller-saved 约定之外的自有协议：
 * - 帧基 rbp（deopt 时由入口桥恢复 rbp/rsp 快照，见 jit_runtime.cj）
 * - 16B 对齐（call 前 rsp % 16 == 0）
 *
 * 寄存器编码（SysV GPR 编号）：rax=0 rcx=1 rdx=2 rbx=3 rsp=4 rbp=5 rsi=6 rdi=7 r8=8 ...
 */
public class JitEmitter {
    var buf: ArrayList<UInt8> = ArrayList<UInt8>()

    public func b(v: UInt8): Unit {
        this.buf.add(v)
    }

    // imm32（小端；用于 SUB/ADD rsp、mov r/m64, imm32 等）
    public func w32(v: Int64): Unit {
        this.buf.add(UInt8(v & 0xFF))
        this.buf.add(UInt8((v >> 8) & 0xFF))
        this.buf.add(UInt8((v >> 16) & 0xFF))
        this.buf.add(UInt8((v >> 24) & 0xFF))
    }

    public func here(): Int64 {
        Int64(this.buf.size)
    }

    public func patchRel32(at: Int64, target: Int64): Unit {
        // rel32 字段位于 at（绝对偏移），目标为绝对偏移，rel = target - (at + 4)
        let rel = target - (at + 4)
        this.buf[at] = UInt8(rel & 0xFF)
        this.buf[at + 1] = UInt8((rel >> 8) & 0xFF)
        this.buf[at + 2] = UInt8((rel >> 16) & 0xFF)
        this.buf[at + 3] = UInt8((rel >> 24) & 0xFF)
    }

    // ============ 常用指令 ============

    public func pushReg(reg: Int64): Unit {
        if (reg >= 8) {
            this.buf.add(0x41)   // REX.B
        }
        // push 操作码域仅 0x50+rd(3bit)；reg=12 直接 0x50+12=0x5C 会落入 pop 编码域
        this.buf.add(UInt8(0x50 + (reg & 7)))
    }

    public func popReg(reg: Int64): Unit {
        if (reg >= 8) {
            this.buf.add(0x41)
        }
        this.buf.add(UInt8(0x58 + (reg & 7)))
    }

    // mov r64, r/m64：REX.W + 8B /r（reg=dst, rm=src）
    public func movRegRm(dst: Int64, src: Int64): Unit {
        this.rexW(src, dst)
        this.buf.add(0x8B)
        this.buf.add(UInt8(0xC0 + 8 * (dst & 7) + (src & 7)))
    }

    // mov r/m64, r64：REX.W + 89 /r（reg=src, rm=dst）
    public func movRmReg(dst: Int64, src: Int64): Unit {
        this.rexW(dst, src)
        this.buf.add(0x89)
        this.buf.add(UInt8(0xC0 + 8 * (src & 7) + (dst & 7)))
    }

    // mov r9, [r10 + rax*8]：4D 8B 0C C2（REX.WRB；SIB scale=8 index=rax base=r10；reg=001→r9）
    public func movR9PtrR10X(): Unit {
        this.buf.add(0x4D); this.buf.add(0x8B); this.buf.add(0x0C); this.buf.add(0xC2)
    }

    // mov r9, [mem] 备用……（当前用 movR9PtrR10X）
    public func movR11R9(): Unit {
        this.buf.add(0x4D); this.buf.add(0x89); this.buf.add(0xD9)
    }

    // mov r/m64, imm32（符号扩展）：REX.W + C7 /0 id
    public func movRmImm32(rm: Int64, imm: Int64): Unit {
        this.rexW(rm, 0)
        this.buf.add(0xC7)
        this.buf.add(UInt8(0xC0 + (rm & 7)))
        this.w32(imm)
    }

    // mov r/m64, imm64（rdi/rsi 等绝对地址注入）：REX.W + B8+rd io
    public func movRmImm64(rm: Int64, imm: Int64): Unit {
        this.rexW(rm, rm)
        this.buf.add(UInt8(0xB8 + (rm & 7)))
        for (i in 0..8) {
            this.buf.add(UInt8((imm >> (8 * i)) & 0xFF))
        }
    }

    // add r64, r/m64：REX.W + 01 /r（reg=src, rm=dst）
    public func addRmReg(dst: Int64, src: Int64): Unit {
        this.rexW(dst, src)
        this.buf.add(0x01)
        this.buf.add(UInt8(0xC0 + 8 * (src & 7) + (dst & 7)))
    }

    // sub r/m64, r64：REX.W + 29 /r（reg=src, rm=dst）
    public func subRmReg(dst: Int64, src: Int64): Unit {
        this.rexW(dst, src)
        this.buf.add(0x29)
        this.buf.add(UInt8(0xC0 + 8 * (src & 7) + (dst & 7)))
    }

    // cmp r/m64, r64：REX.W + 39 /r
    public func cmpRmReg(a: Int64, b: Int64): Unit {
        this.rexW(a, b)
        this.buf.add(0x39)
        this.buf.add(UInt8(0xC0 + 8 * (b & 7) + (a & 7)))
    }

    // cmp r/m64, imm32：REX.W + 81 /7 id
    public func cmpRmImm32(rm: Int64, imm: Int64): Unit {
        this.rexW(rm, 0)
        this.buf.add(0x81)
        this.buf.add(UInt8(0xF8 + (rm & 7)))
        this.w32(imm)
    }

    // add r/m64, imm32：REX.W + 81 /0 id
    public func addRmImm32(rm: Int64, imm: Int64): Unit {
        this.rexW(rm, 0)
        this.buf.add(0x81)
        this.buf.add(UInt8(0xC0 + (rm & 7)))
        this.w32(imm)
    }

    // sub r/m64, imm32：REX.W + 81 /5 id
    public func subRmImm32(rm: Int64, imm: Int64): Unit {
        this.rexW(rm, 0)
        this.buf.add(0x81)
        this.buf.add(UInt8(0xE8 + (rm & 7)))
        this.w32(imm)
    }

    // sub rsp, imm32：48 81 EC id
    public func subRspImm32(imm: Int64): Unit {
        this.buf.add(0x48); this.buf.add(0x81); this.buf.add(0xEC)
        this.w32(imm)
    }

    // add rsp, imm32：48 81 C4 id
    public func addRspImm32(imm: Int64): Unit {
        this.buf.add(0x48); this.buf.add(0x81); this.buf.add(0xC4)
        this.w32(imm)
    }

    public func pushRbp(): Unit { this.pushReg(5) }
    public func popRbp(): Unit { this.popReg(5) }
    public func movRbpRsp(): Unit { this.movRegRm(5, 4) }
    public func movRspRbp(): Unit { this.movRegRm(4, 5) }

    // mov reg, [base + disp]：REX.W + 8B /r（modrm 由 emitModrm 处理，含 SIB/disp）
    public func movRegRmDisp(dst: Int64, base: Int64, disp: Int64): Unit {
        this.rexW(base, dst)
        this.buf.add(0x8B)
        this.emitModrm(base, disp, dst)
    }

    // mov [base + disp], reg：89 /r
    public func movRmDispReg(base: Int64, disp: Int64, src: Int64): Unit {
        this.rexW(base, src)
        this.buf.add(0x89)
        this.emitModrm(base, disp, src)
    }

    // 通用 modrm 发射（带 reg 字段；之后自动补 SIB/disp 字节）
    func emitModrm(base: Int64, disp: Int64, reg: Int64): Unit {
        let mod: Int64 = if (disp == 0 && base != 5 && base != 4) { 0 } else if (disp >= -128 && disp <= 127) { 1 } else { 2 }
        if (base == 4) {
            // rsp 基址：rm=100 + SIB=0x24
            this.buf.add(UInt8((mod << 6) | ((reg & 7) << 3) | 0x04))
            this.buf.add(0x24)
            if (mod == 1) {
                this.buf.add(UInt8(disp & 0xFF))
            } else if (mod == 2) {
                this.w32(disp)
            }
        } else {
            this.buf.add(UInt8((mod << 6) | ((reg & 7) << 3) | (base & 7)))
            if (mod == 1) {
                this.buf.add(UInt8(disp & 0xFF))
            } else if (mod == 2) {
                this.w32(disp)
            }
        }
    }

    // push imm32（对齐 pad 用）：68 id
    public func pushImm32(imm: Int64): Unit {
        this.buf.add(0x68)
        this.w32(imm)
    }

    // lea reg, [base + disp]：REX.W + 8D /r
    public func leaRegRmDisp(dst: Int64, base: Int64, disp: Int64): Unit {
        this.rexW(base, dst)
        this.buf.add(0x8D)
        this.emitModrm(base, disp, dst)
    }

    // call 绝对地址：mov r11, imm64 (49 BB iq) + call r11 (41 FF D3)
    public func callAbs(addr: Int64): Unit {
        this.buf.add(0x49); this.buf.add(0xBB)
        for (i in 0..8) {
            this.buf.add(UInt8((addr >> (8 * i)) & 0xFF))
        }
        this.buf.add(0x41); this.buf.add(0xFF); this.buf.add(0xD3)
    }

    // call rel32（内部函数调用）：E8 id
    public func callRel32(): Int64 {
        this.buf.add(0xE8)
        let at = this.here()
        this.w32(0)
        at
    }

    // jmp rel32：E9 id
    public func jmpRel32(): Int64 {
        this.buf.add(0xE9)
        let at = this.here()
        this.w32(0)
        at
    }

    // jcc rel32（0F 8x cd）：condByte 为完整字节（0x85=jne 0x84=je 0x8C=jl 0x8D=jnl 0x8E=jng 0x8F=jg）
    public func jccRel32(condByte: Int64): Int64 {
        this.buf.add(0x0F)
        this.buf.add(UInt8(condByte))
        let at = this.here()
        this.w32(0)
        at
    }

    // ret：C3
    public func ret(): Unit {
        this.buf.add(0xC3)
    }

    public func size(): Int64 {
        Int64(this.buf.size)
    }

    public func bytes(): ArrayList<UInt8> {
        this.buf
    }

    func rexW(rmReg: Int64, reg: Int64): Unit {
        var rex: Int64 = 0x48
        if (rmReg >= 8) { rex = rex | 0x01 }
        if (reg >= 8) { rex = rex | 0x04 }
        if (rex != 0x48) {
            this.buf.add(UInt8(rex))
        } else {
            this.buf.add(0x48)
        }
    }
}
