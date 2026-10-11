package ystyle::xisp_tests

import ystyle::xisp.*
import ystyle::xisp_bc.*
import ystyle::xisp.types.*
import ystyle::xisp.core.eval.*
import ystyle::xisp.bridge.*
import ystyle::xisp.core.builtin.*
import std.collection.ArrayList

import std.unittest.*
import std.unittest.testmacro.*

/**
 * 外部值接缝（能力域设计 §7，接缝 3）
 *
 * ## 为什么 core 必须开这个口子
 *
 * `LispValue` 是封闭枚举。外部模块（NDArray / 分布对象 / 句柄…）要把自己的值
 * 交给 Lisp 层，就必须有一个 core 承认的值形态；否则只能退化成 `HashMap`/`Cons`：
 * 1e6 元素 = 1e6 个装箱值，连续缓冲与原生身份全丢，视图/切片无法零拷贝，
 * FFI 指针无处安放。
 *
 * ## 用测试用假扩展端到端验证
 *
 * 本测试用一个极简外部类型 `CounterBox`（带长度的外部计数器盒子）走通全链路：
 * 注册域 → 产生 External → `type-of` / `external?` / `length`（能力接口分派）/
 * `print`（display）/ 相等性。它是「先验证接缝、再开真实扩展族」这一步的凭证。
 */

/** 测试用外部值：一个定长计数器盒子 */
class CounterBox <: LispExternal & LispExternalIndexable & LispExternalEquatable {
    let cap: Int64

    init(cap: Int64) {
        this.cap = cap
    }

    public prop typeName: String {
        get() { "counter-box" }
    }

    /** 实现方负责截断（core 统一截断会让"想打全量"无法表达） */
    public func display(): String {
        "#<counter-box cap=${this.cap}>"
    }

    public func length(): Int64 {
        this.cap
    }

    public func at(index: Int64): LispValue {
        if (index >= 0 && index < this.cap) {
            LispValue.Int(index)
        } else {
            Nil
        }
    }

    public func equals(other: LispExternal): Bool {
        match (other) {
            case o: CounterBox => o.cap == this.cap
            case _ => false
        }
    }
}

@Test
class ExternalValueTest {
    /** 造一个装了外部值的解释器 */
    func makeIt(): LispInterpreter {
        let it = LispInterpreter([withQuietMode(), withStdLib()])
        let env = it.getEnvironment()
        // 模拟"扩展模块注册自己的域"：用 CapabilityContext 的 register/evalSource
        Bridge.registerFunc(env, "make-counter", { args =>
            let cap = match (args[0]) {
                case LispValue.Int(n) => n
                case _ => 0
            }
            LispValue.External(CounterBox(cap))
        })
        it
    }

    func evalIn(it: LispInterpreter, code: String): String {
        it.eval(code).toString()
    }

    @TestCase
    func testExternalIsRecognized() {
        println("\n=== 外部值被 core 承认 ===")
        let it = this.makeIt()
        @Assert(this.evalIn(it, "(external? (make-counter 5))") == "true")
        println("✓ (external? …) = true")
        @Assert(this.evalIn(it, "(external? 42)") == "false")
        println("✓ 普通值不为 external")
    }

    @TestCase
    func testTypeNameFromImplementer() {
        println("\n=== type-of 报告实现方声明的类型名 ===")
        let it = this.makeIt()
        // 外部值报告自己的 typeName，而非笼统的 "external"
        @Assert(this.evalIn(it, "(type-of (make-counter 5))") == "\"counter-box\"")
        println("✓ (type-of …) = counter-box")
        @Assert(this.evalIn(it, "(external-type (make-counter 5))") == "\"counter-box\"")
        println("✓ (external-type …) = counter-box")
    }

    @TestCase
    func testDisplayFromImplementer() {
        println("\n=== print 走实现方的 display() ===")
        let it = this.makeIt()
        let r = this.evalIn(it, "(str (make-counter 7))")
        @Assert(r.contains("counter-box"))
        @Assert(r.contains("cap=7"))
        println("✓ (str …) = " + r)
    }

    /**
     * **能力接口分派**：core 的 `length` 不认识 CounterBox，
     * 只探测它是否实现 `LispExternalIndexable`。
     */
    @TestCase
    func testLengthDispatchesOnCapability() {
        println("\n=== length 按能力接口分派（core 不认识具体类型） ===")
        let it = this.makeIt()
        @Assert(this.evalIn(it, "(length (make-counter 5))") == "5")
        println("✓ (length (make-counter 5)) = 5")
        // 对照：普通列表仍正常
        @Assert(this.evalIn(it, "(length (quote (1 2 3)))") == "3")
        println("✓ (length '(1 2 3)) = 3")
        // 未实现能力接口的外部值应明确报错，而不是静默返回 0
        let r = this.evalIn(it, "(length 42)")
        @Assert(r.contains("TypeError"))
        println("✓ 非列表非可索引值报 TypeError（此前静默返回 0）")
    }

    @TestCase
    func testEqualityViaCapability() {
        println("\n=== 相等比较可按能力接口实现 ===")
        let it = this.makeIt()
        let box = CounterBox(3)
        // 同一性：同一个对象
        @Assert(box.equals(CounterBox(3)))
        @Assert(!box.equals(CounterBox(4)))
        println("✓ CounterBox(3) == CounterBox(3)，!= CounterBox(4)")
    }

    /**
     * 外部值在**两个后端**下行为一致。
     *
     * 这条尤其重要：External 是新变体，若编译器/VM 未正确处理会在字节码下崩。
     */
    @TestCase
    func testBothBackendsAgree() {
        println("\n=== 双后端一致 ===")
        for (useBc in [false, true]) {
            let opts = ArrayList<InterpreterOption>()
            opts.add(withQuietMode())
            opts.add(withStdLib())
            if (useBc) {
                opts.add(withBytecodeCompiler())
            }
            let it = LispInterpreter(opts.toArray())
            Bridge.registerFunc(it.getEnvironment(), "make-counter", { args =>
                let cap = match (args[0]) {
                    case LispValue.Int(n) => n
                    case _ => 0
                }
                LispValue.External(CounterBox(cap))
            })
            @Assert(this.evalIn(it, "(external? (make-counter 1))") == "true")
            @Assert(this.evalIn(it, "(type-of (make-counter 1))") == "\"counter-box\"")
            @Assert(this.evalIn(it, "(length (make-counter 9))") == "9")
        }
        println("✓ AST 与字节码下 external? / type-of / length 均正确")
    }

    /** 外部值可经域装配（CapabilityContext.register）安装 */
    @TestCase
    func testInstallViaCapabilityContext() {
        println("\n=== 经能力域安装外部值 ===")
        let domain = SimpleCapability(
            "demo.counterbox",
            ["make-box"],
            { ctx =>
                ctx.register("make-box", { args =>
                    let cap = match (args[0]) {
                        case LispValue.Int(n) => n
                        case _ => 0
                    }
                    LispValue.External(CounterBox(cap))
                })
                ctx.registerType("counter-box")
            },
            escapable: false, hostPolicy: false
        )
        // 注意 replace: true 会清空默认装配，故需显式声明用到的域：
        // logic 提供 `external?`/`type-of`，list 提供 `length`
        let it = LispInterpreter([withQuietMode(), withDomains([
            BuiltinFunctions.domainBase(),
            BuiltinFunctions.domainLogic(),
            BuiltinFunctions.domainList(),
            domain,
        ], replace: true)])
        @Assert(this.evalIn(it, "(external? (make-box 4))") == "true")
        @Assert(this.evalIn(it, "(length (make-box 4))") == "4")
        println("✓ 域安装的 make-box 可用，返回外部值且 length 分派正确")
    }
}
