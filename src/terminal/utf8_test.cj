package ystyle::xisp.terminal

import std.collection.ArrayList
import std.unittest.*
import std.unittest.testmacro.*

/**
 * 终端 UTF-8 多字节解码测试（中文/emoji 输入不拆字）
 *
 * 背景：终端 readKey 原把原始字节逐个当 Rune（Byte=UInt8），中文"中"(E4 B8 AD) 会被拆成
 * U+00E4/U+00B8/U+00AD 三个错字符；跨 read 边界时还有"半个中文"被拆的风险。
 * 修复：utf8SeqLen（由引导字节推断序列长）+ decodeUtf8Rune（多字节解码 + 续字节校验，
 * 非法 → U+FFFD 替换符）+ readKey 跨边界补齐。
 */
@Test
class Utf8Test {
    func seq(bytes: Array<Int64>): ArrayList<Byte> {
        let r = ArrayList<Byte>()
        for (b in bytes) {
            r.add(UInt8(b))
        }
        r
    }

    @TestCase
    func testUtf8SeqLen() {
        // ASCII / 2 字节(é) / 3 字节(中) / 4 字节(emoji)
        @Assert(utf8SeqLen(UInt8(0x41)) == 1)
        @Assert(utf8SeqLen(UInt8(0xC3)) == 2)
        @Assert(utf8SeqLen(UInt8(0xE4)) == 3)
        @Assert(utf8SeqLen(UInt8(0xF0)) == 4)
        // 非法引导：孤立续字节(0x80) / 过窄(0xC0) / 超范围(0xF5) → 按单字节容错
        @Assert(utf8SeqLen(UInt8(0x80)) == 1)
        @Assert(utf8SeqLen(UInt8(0xC0)) == 1)
        @Assert(utf8SeqLen(UInt8(0xF5)) == 1)
    }

    @TestCase
    func testDecodeChinese() {
        // 中 = E4 B8 AD → U+4E2D；你 = E4 BD A0 → U+4F60
        let r1 = decodeUtf8Rune(this.seq([0xE4, 0xB8, 0xAD]))
        let r2 = decodeUtf8Rune(this.seq([0xE4, 0xBD, 0xA0]))
        @Assert(r1 == Rune(UInt32(0x4E2D)))
        @Assert(r2 == Rune(UInt32(0x4F60)))
    }

    @TestCase
    func testDecodeLatinAndEmoji() {
        // é = C3 A9 → U+00E9；😀 = F0 9F 98 80 → U+1F600（4 字节代理对）
        let r1 = decodeUtf8Rune(this.seq([0xC3, 0xA9]))
        let r2 = decodeUtf8Rune(this.seq([0xF0, 0x9F, 0x98, 0x80]))
        @Assert(r1 == Rune(UInt32(0x00E9)))
        @Assert(r2 == Rune(UInt32(0x1F600)))
    }

    @TestCase
    func testRuneDisplayWidth() {
        // ASCII 单宽 / CJK·emoji 双宽（光标对齐用）
        @Assert(runeDisplayWidth(Rune(UInt32(0x41))) == 1)      // 'A'
        @Assert(runeDisplayWidth(Rune(UInt32(0x4E2D))) == 2)    // 中
        @Assert(runeDisplayWidth(Rune(UInt32(0x1F600))) == 2)   // 😀
        @Assert(runeDisplayWidth(Rune(UInt32(0x00E9))) == 1)    // é（组合前，单宽）
    }

    @TestCase
    func testDecodeInvalid() {
        // 非法续字节 / 长度不足 → U+FFFD 替换符（绝不拆字出乱码）
        let r1 = decodeUtf8Rune(this.seq([0xE4, 0x41, 0xAD]))
        let r2 = decodeUtf8Rune(this.seq([0xE4]))
        let r3 = decodeUtf8Rune(ArrayList<Byte>())
        @Assert(r1 == Rune(UInt32(0xFFFD)))
        @Assert(r2 == Rune(UInt32(0xFFFD)))
        @Assert(r3 == Rune(UInt32(0xFFFD)))
    }
}
