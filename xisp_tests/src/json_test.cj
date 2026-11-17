package ystyle::xisp_tests

import ystyle::xisp.*
import ystyle::xisp_bc.*
import ystyle::xisp_json.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import ystyle::xisp.core.eval.*
import ystyle::xisp.core.builtin.*

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList

/**
 * JSON 能力（能力域 `json`）的契约测试
 *
 * 设计见 `docs/plans/2026-11-17-json-design.md`。这里只用 public API
 * （`withJson()` + Lisp 代码），映射规则的白盒测试在同模块的 `json_test.cj`。
 *
 * 三个后端必须一致 —— JSON 走的是 `NativeFunc`，理论上天然一致，
 * 但 `json-parse` 的返回值是 `HashMap` / `Cons`，要确认 BC / JIT 对这两类值的
 * 后续操作没有分歧。
 */
func evalJsonWithBackend(code: String, backend: String): LispValue {
    let opts = ArrayList<InterpreterOption>()
    opts.add(withQuietMode())
    opts.add(withStdLib())
    opts.add(withJson())
    if (backend != "AST") {
        opts.add(withBytecodeCompiler())
    }
    if (backend == "JIT") {
        opts.add(withJit())
    }
    let it = LispInterpreter(opts.toArray())
    var result: LispValue = Nil
    for (expr in Parser.parseFromString(code)) {
        result = it.evalExpr(expr)
    }
    result
}

func assertJsonSameAllBackends(code: String, expect: String): Unit {
    for (b in backendNames()) {
        let r = evalJsonWithBackend(code, b)
        let k = kindOf(r)
        if (k.size > 0) {
            @Fail("[${b}] 期望 `${expect}`，实际报错 ${k}：${r.toString()}（代码：${code}）")
        }
        if (r.toString() != expect) {
            @Fail("[${b}] 期望 `${expect}`，实际 `${r.toString()}`（代码：${code}）")
        }
    }
}

func assertJsonErrorAllBackends(code: String, expectKind: String): Unit {
    var firstKind = ""
    for (b in backendNames()) {
        let r = evalJsonWithBackend(code, b)
        let k = kindOf(r)
        if (k.isEmpty()) {
            @Fail("[${b}] 期望错误 `${expectKind}`，实际正常值 ${r.toString()}（代码：${code}）")
        }
        if (firstKind.isEmpty()) {
            firstKind = k
        } else if (k != firstKind) {
            @Fail("后端错误种类不一致：${code} → ${firstKind} vs ${b}:${k}")
        }
        if (k != expectKind) {
            @Fail("[${b}] 期望 `${expectKind}`，实际 `${k}`（代码：${code}）")
        }
    }
}

@Test
class JsonTest {
    /** 四个符号都装上了 */
    @TestCase
    func testSymbolsInstalled() {
        println("\n=== 四个 JSON 符号可用 ===")
        for (name in ["cangjie::json-get", "cangjie::json-get-many",
                      "cangjie::json-parse", "cangjie::json-valid?"]) {
            let r = evalJsonWithBackend(
                #"(cangjie::json-valid? "")"#, "AST")
            // 用 procedure? 检查每个符号是一等值
            let check = evalJsonWithBackend("(procedure? ${name})", "AST")
            if (check.toString() != "true") {
                @Fail("${name} 应为 procedure，实得 ${check.toString()}")
            }
            if (kindOf(r).size > 0) {
                @Fail("json-valid? 不应报错：${r.toString()}")
            }
        }
        println("✓ json-get / json-get-many / json-parse / json-valid? 均为可调用值")
    }

    /** `json-get`：对象、数组下标、嵌套路径 */
    @TestCase
    func testJsonGet() {
        println("\n=== json-get 路径取值 ===")
        assertJsonSameAllBackends(#"(cangjie::json-get "{\"a\":1}" "a")"#, "1")
        assertJsonSameAllBackends(#"(cangjie::json-get "{\"a\":\"x\"}" "a")"#, "\"x\"")
        assertJsonSameAllBackends(#"(cangjie::json-get "[10,20,30]" "1")"#, "20")
        assertJsonSameAllBackends(
            #"(cangjie::json-get "{\"u\":{\"name\":\"ann\"}}" "u.name")"#, "\"ann\"")
        assertJsonSameAllBackends(
            #"(cangjie::json-get "{\"l\":[{\"n\":1},{\"n\":2}]}" "l.1.n")"#, "2")
        println("✓ 对象字段 / 数组下标 / 嵌套路径在三个后端一致")
    }

    /**
     * 路径不存在 → `nil`，**不是错误**
     *
     * 与 `hashmap-get` 一致：取不到就是 `nil`，`(if x ...)` 直接可用。
     * 这条如果错了，所有"探测字段是否存在"的脚本都会崩。
     */
    @TestCase
    func testMissingPathIsNilNotError() {
        println("\n=== 路径不存在返回 nil ===")
        assertJsonSameAllBackends(#"(cangjie::json-get "{\"a\":1}" "b")"#, "nil")
        assertJsonSameAllBackends(#"(cangjie::json-get "{\"a\":1}" "a.b.c")"#, "nil")
        assertJsonSameAllBackends(#"(cangjie::json-get "[]" "0")"#, "nil")
        assertJsonSameAllBackends(
            #"(null? (cangjie::json-get "{\"a\":1}" "zzz"))"#, "true")
        println("✓ 不存在的路径一律 nil，null? 判定为真")
    }

    /**
     * `json-get-many`：批量取，顺序与 `paths` 一致
     *
     * 它的价值不只是省事 —— 一次 `valid` + 一次扫描，避免在大 JSON 上
     * 反复调 `json-get` 时每次都重扫全文。
     */
    @TestCase
    func testJsonGetMany() {
        println("\n=== json-get-many 批量取值 ===")
        assertJsonSameAllBackends(
            #"(cangjie::json-get-many "{\"a\":1,\"b\":2,\"c\":3}" '("a" "c" "zzz"))"#,
            "(1 3 nil)")
        assertJsonSameAllBackends(
            #"(length (cangjie::json-get-many "{\"a\":1}" '("a" "b")))"#, "2")
        println("✓ 顺序与 paths 一致，缺的补 nil")
    }

    /**
     * 整数必须仍是 `Int`
     *
     * JSON 不分整数与浮点，但 Lisp 用户分：`(+ n 1)` 得到 `43` 而不是 `43.0`。
     * 这条靠"看 raw 文本形态"实现（`42` → Int，`42.0` / `1e3` → Float）。
     */
    @TestCase
    func testIntegerStaysInteger() {
        println("\n=== 整数不丢整数性 ===")
        assertJsonSameAllBackends(
            #"(+ (cangjie::json-get "{\"n\":42}" "n") 1)"#, "43")
        assertJsonSameAllBackends(
            #"(integer? (cangjie::json-get "{\"n\":42}" "n"))"#, "true")
        // 有小数点 / 指数 → 浮点
        assertJsonSameAllBackends(
            #"(float? (cangjie::json-get "{\"n\":42.0}" "n"))"#, "true")
        assertJsonSameAllBackends(
            #"(float? (cangjie::json-get "{\"n\":1e3}" "n"))"#, "true")
        assertJsonSameAllBackends(
            #"(type-of (cangjie::json-get "{\"n\":7}" "n"))"#, "\"integer\"")
        println("✓ 42 是 integer、42.0 与 1e3 是 float；(+ n 1) = 43")
    }

    /** `json-parse`：转成树，object → HashMap、array → 列表 */
    @TestCase
    func testJsonParseTree() {
        println("\n=== json-parse 解析成树 ===")
        // object 可用 hashmap-get / let 解构
        assertJsonSameAllBackends(
            #"(hashmap-get (cangjie::json-parse "{\"a\":1,\"b\":2}") "b")"#, "2")
        assertJsonSameAllBackends(
            #"(let [{:a x} (cangjie::json-parse "{\"a\":9}")] x)"#, "9")
        // array 是列表
        assertJsonSameAllBackends(
            #"(car (cangjie::json-parse "[7,8]"))"#, "7")
        assertJsonSameAllBackends(
            #"(length (cangjie::json-parse "[7,8,9]"))"#, "3")
        // 嵌套：遍历 + 累加
        assertJsonSameAllBackends(
            #"(reduce + 0 (map (lambda (m) (hashmap-get m "n"))
                            (cangjie::json-parse "[{\"n\":1},{\"n\":2},{\"n\":3}]")))"#,
            "6")
        // 标量
        assertJsonSameAllBackends(#"(cangjie::json-parse "true")"#, "true")
        assertJsonSameAllBackends(#"(cangjie::json-parse "null")"#, "nil")
        assertJsonSameAllBackends(#"(cangjie::json-parse "42.5")"#, "42.500000")
        println("✓ HashMap 可 hashmap-get / let 解构；列表可 car / length / map / reduce")
    }

    /**
     * **空数组与 `null` 都得到 `nil`**
     *
     * xisp 里空列表就是 `nil`（`'()` = `nil`，见 `list()` 的实现），这是语言
     * 既有语义，本模块不为此破例。要区分时用 `json-get` 取具体字段，
     * 或看原始文本。
     */
    @TestCase
    func testEmptyArrayEqualsNil() {
        println("\n=== 空数组 = nil（既有语义）===")
        assertJsonSameAllBackends(#"(cangjie::json-parse "[]")"#, "nil")
        assertJsonSameAllBackends(#"(cangjie::json-parse "null")"#, "nil")
        assertJsonSameAllBackends(#"(null? (cangjie::json-parse "[]"))"#, "true")
        println("✓ [] 与 null 映射后不可区分 —— 已在文档里写明")
    }

    /** `json-valid?`：合法 / 非法 / 非字符串（谓词惯例：非字符串给 false） */
    @TestCase
    func testJsonValid() {
        println("\n=== json-valid? 语法检查 ===")
        assertJsonSameAllBackends(#"(cangjie::json-valid? "{\"a\":1}")"#, "true")
        assertJsonSameAllBackends(#"(cangjie::json-valid? "[1,2]")"#, "true")
        assertJsonSameAllBackends(#"(cangjie::json-valid? "42")"#, "true")
        assertJsonSameAllBackends(#"(cangjie::json-valid? "{bad}")"#, "false")
        assertJsonSameAllBackends(#"(cangjie::json-valid? "")"#, "false")
        // 谓词惯例：非字符串返回 false（与 exists? 一致），不报 TypeError
        assertJsonSameAllBackends("(cangjie::json-valid? 123)", "false")
        assertJsonSameAllBackends("(cangjie::json-valid? nil)", "false")
        println("✓ 合法 true、非法 false、非字符串 false")
    }

    /** 语法非法 → `SyntaxError`（不是 nil，也不是静默成功） */
    @TestCase
    func testSyntaxError() {
        println("\n=== 语法非法报 SyntaxError ===")
        assertJsonErrorAllBackends(#"(cangjie::json-parse "{bad}")"#, "SyntaxError")
        assertJsonErrorAllBackends(#"(cangjie::json-parse "")"#, "SyntaxError")
        assertJsonErrorAllBackends(#"(cangjie::json-get "{bad}" "a")"#, "SyntaxError")
        assertJsonErrorAllBackends(#"(cangjie::json-get-many "{bad}" '("a"))"#, "SyntaxError")
        println("✓ parse / get / get-many 对非法 JSON 都报 SyntaxError")
    }

    /** 参数类型 / 个数错误 */
    @TestCase
    func testArgumentErrors() {
        println("\n=== 参数错误 ===")
        assertJsonErrorAllBackends("(cangjie::json-get 1 \"a\")", "TypeError")
        assertJsonErrorAllBackends("(cangjie::json-get \"{}\" 1)", "TypeError")
        assertJsonErrorAllBackends(#"(cangjie::json-get-many "{}" "a")"#, "TypeError")
        assertJsonErrorAllBackends(#"(cangjie::json-get-many "{}" '(1))"#, "TypeError")
        assertJsonErrorAllBackends("(cangjie::json-parse 1)", "TypeError")
        assertJsonErrorAllBackends(#"(cangjie::json-get "{}")"#, "ArityMismatch")
        assertJsonErrorAllBackends(#"(cangjie::json-parse "{}" "x")"#, "ArityMismatch")
        println("✓ 类型错 TypeError、个数错 ArityMismatch")
    }

    /**
     * **错误会中止求值链**（与 0.6.0 的错误语义一致）
     *
     * `json-parse` 的 `SyntaxError` 不能被静默忽略，否则
     * `(begin (json-parse "坏的") 42)` 会算出 42，让校验形同虚设。
     */
    @TestCase
    func testErrorAbortsChain() {
        println("\n=== JSON 错误中止求值链 ===")
        assertJsonErrorAllBackends(#"(begin (cangjie::json-parse "{bad}") 42)"#, "SyntaxError")
        assertJsonErrorAllBackends(
            #"(begin (define x (cangjie::json-parse "{bad}")) x)"#, "SyntaxError")
        println("✓ begin / define 都不会吞掉 JSON 错误")
    }

    /**
     * 域属性：JSON 是纯计算
     *
     * `escapable = false` / `hostPolicy = false` —— 不碰文件、网络、进程。
     * 装配可以只给 JSON（`replace: true`），此时连 `println` 都没有。
     */
    @TestCase
    func testDomainIsPureAndAssemblable() {
        println("\n=== JSON 域是纯计算且可独占装配 ===")
        let jsonDom = jsonDomain()
        @Assert(!jsonDom.escapable)
        @Assert(!jsonDom.hostPolicy)
        @Assert(jsonDom.name == "json")
        @Assert(jsonDom.symbols.size == 4)

        // replace: true 独占装配：只有 JSON + 显式给的域
        let it = LispInterpreter([
            withQuietMode(),
            withDomains([BuiltinFunctions.domainBase(), jsonDomain()], replace: true),
        ])
        @Assert(it.eval(#"(cangjie::json-get "{\"a\":1}" "a")"#).toString() == "1")
        // 没装 print 域 → println 不可用（证明装配真的独占）
        @Assert(it.eval("(println 1)").toString().contains("UndefinedFunction"))
        println("✓ 纯计算属性正确；独占装配下只有显式声明的域")
    }

    /**
     * **解析结果与手工构造的等价结构一致**
     *
     * 没有 `json-dump`（序列化是第二批），所以"往返"只做单向比对：
     * `json-parse` 出来的树，必须和用 `HashMap` / 列表 / 标量手工搭出来的
     * 等价结构在**显示口径**上一致。
     *
     * 用 `str` 比较而不是 `=` / `eq?` —— 这两个内建只做数值与标量的浅比较
     * （实测 `(= "a" "a")` → `false`、`(eq? (list 1 2) (list 1 2))` → `false`），
     * 对 `HashMap` / 列表一律不等，不能用来验证结构相等。
     */
    @TestCase
    func testParseMatchesHandmadeStructure() {
        println("\n=== 解析结果 ≡ 手工构造的等价结构 ===")
        // 每条都是「完整 Lisp 表达式」，避免在仓颉层做二次转义
        let jsonExprs = [
            #"(str (cangjie::json-parse "{\"a\":1,\"b\":[2,3]}"))"#,
            #"(str (cangjie::json-parse "{\"s\":\"x\",\"n\":null}"))"#,
            #"(str (cangjie::json-parse "[1,[2,3],{\"k\":\"v\"}]"))"#,
            #"(str (cangjie::json-parse "{\"f\":1.5,\"t\":true}"))"#,
        ]
        let handmadeExprs = [
            "(str {:a 1 :b (list 2 3)})",
            #"(str {:s "x" :n nil})"#,
            #"(str (list 1 (list 2 3) {:k "v"}))"#,
            // 布尔字面量是 #t / #f —— xisp 里 `true` / `false` 是未绑定变量
            "(str {:f 1.5 :t #t})",
        ]
        for (i in 0..jsonExprs.size) {
            let fromJson = evalJsonWithBackend(jsonExprs[i], "AST").toString()
            let manual = evalJsonWithBackend(handmadeExprs[i], "AST").toString()
            if (fromJson != manual) {
                @Fail("第 ${i} 组结构与手工构造不一致：json → ${fromJson}，手工 → ${manual}")
            }
        }
        println("✓ 4 组结构（嵌套数组 / object / null / 浮点 / 布尔）与手工构造一致")
    }

    /**
     * **用户文档里的每个示例都必须是真的**
     *
     * `docs-site/api/json.md` 里的 `=> 输出` 全部在这里断言一遍。文档腐化
     * （改了行为没改文档，或写文档时想当然）是常见事故，这条用例把它变成编译期之外的
     * 第二道闸：文档改了但示例过期，这里会红。
     */
    @TestCase
    func testDocExamples() {
        println("\n=== 文档示例逐条核对 ===")
        // 取值
        assertJsonSameAllBackends(#"(cangjie::json-get "{\"a\":1}" "a")"#, "1")
        assertJsonSameAllBackends(#"(cangjie::json-get "{\"a\":[1,2,3]}" "a.1")"#, "2")
        assertJsonSameAllBackends(
            #"(cangjie::json-get "{\"user\":{\"name\":\"ann\"}}" "user.name")"#, "\"ann\"")
        assertJsonSameAllBackends(#"(cangjie::json-get "{\"a\":1}" "zzz")"#, "nil")
        assertJsonSameAllBackends(
            #"(cangjie::json-get-many "{\"a\":1,\"b\":2}" '("a" "b" "zzz"))"#, "(1 2 nil)")
        // 解析
        assertJsonSameAllBackends(#"(cangjie::json-parse "{\"a\":1,\"b\":\"x\"}")"#,
            "{a 1 b \"x\"}")
        assertJsonSameAllBackends(#"(cangjie::json-parse "[1,2,3]")"#, "(1 2 3)")
        assertJsonSameAllBackends(#"(cangjie::json-parse "42")"#, "42")
        assertJsonSameAllBackends(#"(cangjie::json-parse "42.5")"#, "42.500000")
        assertJsonSameAllBackends(#"(cangjie::json-parse "null")"#, "nil")
        assertJsonSameAllBackends(#"(cangjie::json-parse "true")"#, "true")
        assertJsonSameAllBackends(#"(hashmap-get (cangjie::json-parse "{\"a\":1,\"b\":2}") "b")"#, "2")
        assertJsonSameAllBackends(#"(let [{:a x} (cangjie::json-parse "{\"a\":9}")] x)"#, "9")
        assertJsonSameAllBackends(#"(car (cangjie::json-parse "[7,8]"))"#, "7")
        assertJsonSameAllBackends(#"(length (cangjie::json-parse "[7,8,9]"))"#, "3")
        assertJsonSameAllBackends(
            #"(reduce + 0 (map (lambda (m) (hashmap-get m "n"))
                              (cangjie::json-parse "[{\"n\":1},{\"n\":2},{\"n\":3}]")))"#,
            "6")
        // 检查
        assertJsonSameAllBackends(#"(cangjie::json-valid? "{\"a\":1}")"#, "true")
        assertJsonSameAllBackends(#"(cangjie::json-valid? "{bad}")"#, "false")
        assertJsonSameAllBackends("(cangjie::json-valid? 123)", "false")
        // 类型映射与两条边界
        assertJsonSameAllBackends(#"(type-of (cangjie::json-get "{\"n\":7}" "n"))"#, "\"integer\"")
        assertJsonSameAllBackends(#"(+ (cangjie::json-get "{\"n\":42}" "n") 1)"#, "43")
        assertJsonSameAllBackends(
            #"(cangjie::json-get "{\"n\":9223372036854775807}" "n")"#, "9223372036854775807")
        assertJsonSameAllBackends(
            #"(cangjie::json-get "{\"n\":9223372036854775808}" "n")"#, "9223372036854775808.000000")
        assertJsonSameAllBackends(#"(cangjie::json-parse "[]")"#, "nil")
        // 错误
        assertJsonErrorAllBackends(#"(cangjie::json-parse "{bad}")"#, "SyntaxError")
        assertJsonErrorAllBackends(#"(begin (cangjie::json-parse "{bad}") 42)"#, "SyntaxError")
        println("✓ docs-site/api/json.md 的 25 条示例全部与实现一致")
    }

    /** 一个贴近真实用法的综合用例 */
    @TestCase
    func testRealisticUsage() {
        println("\n=== 综合：解析 API 响应风格的数据 ===")
        let code = #"(
            let [{:status code :data items} (cangjie::json-parse
                    "{\"status\":200,\"data\":[{\"name\":\"a\",\"score\":10},{\"name\":\"b\",\"score\":20}]}")]
              (list code
                    (map (lambda (m) (hashmap-get m "name")) items)
                    (reduce + 0 (map (lambda (m) (hashmap-get m "score")) items))))"#
        assertJsonSameAllBackends(code, "(200 (\"a\" \"b\") 30)")
        println("✓ 解构 + 遍历 + 聚合，三段结果都对")
    }
}
