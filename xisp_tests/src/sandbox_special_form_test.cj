package ystyle::xisp_tests

import ystyle::xisp.*
import ystyle::xisp_bc.*
import ystyle::xisp.types.*
import std.collection.ArrayList

import std.unittest.*
import std.unittest.testmacro.*

/**
 * 权限检查对**特殊形式**的覆盖（能力域设计 §5.4 接缝 4）
 *
 * ## 修复的问题
 *
 * `funcChecker` 此前只挂在普通函数调用路径（`evalFunctionCallWithValue`），
 * 而①语言原语与②求值器拦截**从不经过那里**。于是：
 *
 * | 符号 | 类别 | 修复前拉黑后 |
 * |---|---|---|
 * | `length` | ③注册函数 | ✅ 被拦住 |
 * | `eval` / `apply` | ② | ❌ **照常执行** |
 * | `map` / `filter` / `reduce` / `for-each` / `->` | ② | ❌ 照常执行 |
 *
 * `withSandbox()` 明确写着 `blockFunctions(["eval", "apply"])` —— 两者都拦不住，
 * 沙箱的核心安全意图失效。
 *
 * ## 覆盖范围
 *
 * 只拦**可逃逸**的特殊形式（②类，与"可让位"集合同一批名字）；
 * **不拦** `if` / `define` / `lambda` / `let` 等语言原语 —— 它们是语法结构，
 * 拦掉会让任何脚本都无法运行，那不是"限制能力"而是"语言不可用"。
 */
@Test
class SandboxSpecialFormTest {
    func withBlocked(blocked: Array<String>, code: String): String {
        let it = LispInterpreter([withQuietMode()])
        it.blockFunctions(blocked)
        it.eval(code).toString()
    }

    /** `withSandbox()` 内置拉黑 `eval` / `apply` —— 现在真的生效 */
    @TestCase
    func testSandboxBlocksEvalAndApply() {
        println("\n=== withSandbox 的 blockFunctions([\"eval\",\"apply\"]) 生效 ===")
        let it = LispInterpreter([withQuietMode(), withSandbox()])
        let e1 = it.eval("(eval (quote (+ 1 2)))").toString()
        @Assert(e1.contains("is not allowed"))
        println("✓ (eval …) 被拦住: " + e1)

        let it2 = LispInterpreter([withQuietMode(), withSandbox()])
        let e2 = it2.eval("(apply + (quote (1 2)))").toString()
        @Assert(e2.contains("is not allowed"))
        println("✓ (apply …) 被拦住: " + e2)
    }

    /** ②类全部可被拉黑 */
    @TestCase
    func testBlocksAllEscapableSpecialForms() {
        println("\n=== ②类可逃逸形式均可拉黑 ===")
        let cases: Array<(String, String)> = [
            ("map", "(map (lambda (x) x) (quote (1 2)))"),
            ("filter", "(filter (lambda (x) #t) (quote (1 2)))"),
            ("reduce", "(reduce + 0 (quote (1 2)))"),
            ("apply", "(apply + (quote (1 2)))"),
            ("eval", "(eval (quote (+ 1 2)))"),
            ("for-each", "(for-each (lambda (x) x) (quote (1)))"),
            ("->", "(-> 1 (+ 1))"),
        ]
        for ((name, code) in cases) {
            let r = this.withBlocked([name], code)
            @Assert(r.contains("is not allowed"))
            println("✓ 拉黑 " + name + " 后调用被拒")
        }
    }

    /**
     * 语言原语**不受**权限策略影响。
     *
     * 它们是语法结构而非"能力"：若连 `if`/`let` 都能拦，脚本将完全无法执行。
     */
    @TestCase
    func testLanguagePrimitivesUnaffected() {
        println("\n=== 语言原语不受 funcChecker 影响 ===")
        let blocked = ["if", "define", "lambda", "let", "let*", "quote", "begin", "set!", "match", "condb"]
        let cases: Array<(String, String)> = [
            ("(if #t 1 2)", "1"),
            ("(let ((x 1)) x)", "1"),
            ("(begin 1 2)", "2"),
            ("(match 1 (1 1) (else 0))", "1"),
            ("(condb #t 1 else 2)", "1"),
        ]
        for ((code, expected) in cases) {
            let r = this.withBlocked(blocked, code)
            @Assert(r == expected)
            println("✓ " + code + " = " + r + "（未被拦）")
        }
    }

    /** 未设置 funcChecker 时不得引入任何行为变化 */
    @TestCase
    func testNoCheckerMeansNoRestriction() {
        println("\n=== 未设 funcChecker 时无限制 ===")
        let it = LispInterpreter([withQuietMode(), withStdLib()])
        @Assert(it.eval("(map (lambda (x) x) (quote (1 2)))").toString() == "(1 2)")
        @Assert(it.eval("(eval (quote (+ 1 2)))").toString() == "3")
        println("✓ map / eval 正常可用")
    }

    /** 双后端一致：拉黑后行为相同 */
    @TestCase
    func testBothBackendsAgree() {
        println("\n=== 拉黑行为在两个后端一致 ===")
        for (useBc in [false, true]) {
            let opts = ArrayList<InterpreterOption>()
            opts.add(withQuietMode())
            if (useBc) {
                opts.add(withBytecodeCompiler())
            }
            let it = LispInterpreter(opts.toArray())
            it.blockFunctions(["map", "eval"])
            let r1 = it.eval("(map (lambda (x) x) (quote (1 2)))").toString()
            let r2 = it.eval("(eval (quote 1))").toString()
            @Assert(r1.contains("is not allowed"))
            @Assert(r2.contains("is not allowed"))
        }
        println("✓ AST 与字节码下均被拒")
    }
}
