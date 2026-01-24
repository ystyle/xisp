package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import ystyle::xisp.types.*
import ystyle::xisp.core.*
import std.collection.ArrayList

/**
 * Procedure 类型测试
 * 测试闭包和函数对象的创建
 *
 * 注意：需要 Evaluator.applyProcedure 的测试已在 evaluator_test.cj 中覆盖
 */
@Test
class ProcedureTest {
    /**
     * 测试过程构造
     */
    @TestCase
    func testProcedureCreation() {
        let env = Environment()
        let params = ArrayList<String>()
        params.add("x")
        params.add("y")
        let body = LispValue.Symbol("x")

        let proc = Procedure(params, body, env)
        match (proc) {
            case Procedure(p, b, e) => @Assert(p.size == 2)
            case _ => @Fail("Expected Procedure")
        }
    }

    /**
     * 测试过程在 LispValue 中
     */
    @TestCase
    func testProcedureInLispValue() {
        let env = Environment()
        let params = ArrayList<String>()
        params.add("x")
        let body = LispValue.Symbol("x")

        let procValue = Procedure(params, body, env)
        @Assert(procValue.isProcedure())
        @Assert(procValue.toString() == "#<procedure>")
    }

    /**
     * 测试过程克隆
     */
    @TestCase
    func testProcedureClone() {
        let env = Environment()
        env.define("x", LispValue.Int(42))

        let params = ArrayList<String>()
        params.add("x")
        let body = LispValue.Symbol("x")

        let procValue = Procedure(params, body, env)
        let cloned = procValue.cloneValue()

        @Assert(cloned.isProcedure())
    }
}
