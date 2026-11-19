package ystyle::xisp.types

import std.collection.ArrayList

/**
 * 外部值协议族（能力域设计 §7）
 *
 * ## 为什么需要
 *
 * `LispValue` 是**封闭枚举**，外部模块（NDArray、分布对象、句柄…）要把自己的值
 * 交给 Lisp 层，就必须有一个 core 承认的值形态。没有它，外部值只能退化成
 * `HashMap` / `Cons`：1e6 元素 = 1e6 个装箱 `LispValue`，连续缓冲与原生身份全丢，
 * 视图/切片无法零拷贝，FFI 指针无处安放。
 *
 * 而 `LispConvertible` / `LispDeserializable` / `ExtendLispValue` 是**转换协议**
 * —— 它们把仓颉类型映射到现有变体（或从其一转回），**不能新增变体**。
 *
 * ## 设计原则：core 只强制最小集，其余按能力接口渐进支持
 *
 * | 接口 | 必需？ | 作用 |
 * |---|---|---|
 * | `LispExternal` | **必需** | `typeName` + `display()`，core 的通用逻辑只依赖这两项 |
 * | `LispExternalEquatable` | 按需 | `=` 的语义；未实现则不可作 `HashMap` 键 |
 * | `LispExternalIndexable` | 按需 | 供 `length` / `nth` 等通用内建分派 |
 * | `LispExternalComparable` | 按需 | 供 `<` / `>` 等通用内建分派 |
 * | `LispExternalCallable` | 按需 | 让外部值能被"调用" |
 *
 * 扩展方负担最小；将来新增能力接口也不会破坏已有实现（它们只是不实现新接口）。
 *
 * ## 语义约定（实现方须遵守，见设计文档 §7.4）
 *
 * 1. `=`：**同一性 + 结构浅比较**，不做逐元素（逐元素用显式 `array-equal?`）；
 * 2. truthy：外部值**恒为真**（对象存在即真），判空用显式谓词；
 * 3. 可变外部值**禁止**作 `HashMap` 键（未实现 `LispExternalEquatable` 就不能作键）；
 * 4. FFI 资源由实现方自行实现 `Resource`，**core 不引入 `close` 语义**；
 * 5. 外部值**不参与**序列化；需要时实现 `LispConvertible.toLisp()` 显式转换；
 * 6. `display()` 的截断由**实现方**负责（core 统一截断会让"想打全量"无法表达）。
 */
public interface LispExternal {
    /** 类型名，如 `"ndarray"`。供 `type-of` / 谓词 / 错误信息使用 */
    prop typeName: String

    /**
     * 文本表示（repr）。
     *
     * **实现方负责截断** —— 见接口头注释第 6 条。
     */
    func display(): String
}

/** 支持相等比较的外部值（未实现则不能作 `HashMap` 键） */
public interface LispExternalEquatable <: LispExternal {
    func equals(other: LispExternal): Bool
}

/** 支持按下标访问的外部值：供 `(length x)` / `(nth x i)` 等通用内建分派 */
public interface LispExternalIndexable <: LispExternal {
    func length(): Int64
    func at(index: Int64): LispValue
}

/** 支持排序比较的外部值：供 `<` / `>` 等通用内建分派 */
public interface LispExternalComparable <: LispExternal {
    func compare(other: LispExternal): Int64
}

/** 可被调用的外部值（如把某个外部对象当函数用） */
public interface LispExternalCallable <: LispExternal {
    func invoke(args: ArrayList<LispValue>): LispValue
}

/**
 * 支持四则运算的外部值：供 `+` / `-` / `*` / `/` 分派
 *
 * 两个方向分开声明，因为减法与除法不满足交换律：`(- 1 arr)` 和 `(- arr 1)` 是
 * 两个不同的表达式，不能靠"传个布尔标志"让实现方自己倒过来。
 *
 * **返回 `None` 表示"这个组合我不支持"**，core 会照常报 `TypeError` ——
 * 这样"类型不兼容"和"算错"能区分开，不会静默产出错误结果。
 *
 * 参数类型刻意用 `String` 而不是枚举：外部值将来要支持 `**` / `@`（矩阵乘）
 * 这类新运算符时不必改 core 的接口定义。不认识的 op 应当返回 `None`。
 */
public interface LispExternalArithmetic <: LispExternal {
    /** 本值作为**左**操作数：`(op self other)` */
    func arithLeft(op: String, other: LispValue): Option<LispValue>

    /** 本值作为**右**操作数：`(op other self)` */
    func arithRight(op: String, other: LispValue): Option<LispValue>
}

/**
 * 支持归约的外部值：供 `sum` / `product` / `max` / `min` 分派
 *
 * 不复用 `LispExternalIndexable` 逐元素取值，是因为那会把每个元素装箱成
 * `LispValue` —— 一百万次求和就是一百万次装箱。归约必须由实现方在自己的
 * 连续存储上直接做。
 *
 * `op` 取 `"sum"` / `"product"` / `"max"` / `"min"`；不认识的返回 `None`。
 */
public interface LispExternalReducible <: LispExternal {
    func reduceAll(op: String): Option<LispValue>
}

/**
 * 支持逐元素应用标量函数的外部值：供 `sin` / `exp` / `sqrt` … 分派
 *
 * `fn` 是标量内建的名字。这样 `(sin 1.0)` 与 `(sin arr)` 是**同一个符号**，
 * 不需要一套 `sin` 加一套 `array-sin`。实现方不认识的 `fn` 返回 `None`。
 */
public interface LispExternalElementwise <: LispExternal {
    func applyElementwise(fn: String): Option<LispValue>
}
