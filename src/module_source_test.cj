package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import ystyle::xisp.types.*
import ystyle::xisp.core.*
import std.collection.HashMap
import std.collection.ArrayList

/**
 * ModuleSource 系统测试
 *
 * 测试 MemorySource、ModuleSource 接口和集成到 LispInterpreter
 */
@Test
class ModuleSourceTest {
    /**
     * 测试 MemorySource 基本功能 - 注册和获取元数据
     */
    @TestCase
    func testMemorySourceGetMetadata() {
        let source = MemorySource()
        source.registerModule(
            "test::mod",
            "(module mod (version \"1.0\"))",
            HashMap<String, String>([("core.lisp", "(define foo 42)")])
        )

        let metadata = source.getMetadata("test::mod")
        @Assert(metadata.isSome(), true)
        if (let Some(m) <- metadata) {
            @Assert(m.contains("module mod"), true)
        }
    }

    /**
     * 测试 MemorySource - 获取不存在的模块元数据
     */
    @TestCase
    func testMemorySourceGetMetadataNotFound() {
        let source = MemorySource()
        let metadata = source.getMetadata("nonexistent::mod")
        @Assert(metadata.isNone(), true)
    }

    /**
     * 测试 MemorySource - 列出文件
     */
    @TestCase
    func testMemorySourceListFiles() {
        let source = MemorySource()
        source.registerModule(
            "test::mod",
            "(module mod (version \"1.0\"))",
            HashMap<String, String>([
                ("core.lisp", "(define foo 42)"),
                ("utils.lisp", "(define bar 100)")
            ])
        )

        let files = source.listFiles("test::mod")
        @Assert(files.size == 2, true)
        @Assert(files.contains("core.lisp"), true)
        @Assert(files.contains("utils.lisp"), true)
    }

    /**
     * 测试 MemorySource - 读取文件内容
     */
    @TestCase
    func testMemorySourceReadFile() {
        let source = MemorySource()
        source.registerModule(
            "test::mod",
            "(module mod (version \"1.0\"))",
            HashMap<String, String>([("core.lisp", "(define foo 42)")])
        )

        match (source.readFile("test::mod", "core.lisp")) {
            case Some(content) =>
                @Assert(content, "(define foo 42)")
            case None =>
                @Assert(false, true)
        }
    }

    /**
     * 测试 MemorySource - 读取不存在的文件
     */
    @TestCase
    func testMemorySourceReadFileNotFound() {
        let source = MemorySource()
        source.registerModule(
            "test::mod",
            "(module mod (version \"1.0\"))",
            HashMap<String, String>([("core.lisp", "(define foo 42)")])
        )

        let content = source.readFile("test::mod", "missing.lisp")
        @Assert(content.isNone(), true)
    }

    /**
     * 测试 MemorySource - 不存在的模块文件列表
     */
    @TestCase
    func testMemorySourceListFilesNotFound() {
        let source = MemorySource()
        let files = source.listFiles("nonexistent::mod")
        @Assert(files.isEmpty(), true)
    }

    /**
     * 测试 MemorySource - 批量注册模块
     */
    @TestCase
    func testMemorySourceRegisterModules() {
        let source = MemorySource()
        let modules = HashMap<String, ModuleData>()

        let files1 = HashMap<String, String>([("core.lisp", "(define x 1)")])
        modules["org::mod1"] = ModuleData("org::mod1", "1.0", "org",
            "(module mod1 (version \"1.0\"))", files1)

        let files2 = HashMap<String, String>([("core.lisp", "(define y 2)")])
        modules["org::mod2"] = ModuleData("org::mod2", "2.0", "org",
            "(module mod2 (version \"2.0\"))", files2)

        source.registerModules(modules)

        @Assert(source.getMetadata("org::mod1").isSome(), true)
        @Assert(source.getMetadata("org::mod2").isSome(), true)
        @Assert(source.listFiles("org::mod1").size == 1, true)
        @Assert(source.listFiles("org::mod2").size == 1, true)
    }

    /**
     * 测试 MemorySource - 添加单个文件
     */
    @TestCase
    func testMemorySourceAddFile() {
        let source = MemorySource()
        source.registerMetadata("test::mod", "(module mod (version \"1.0\"))")
        source.addFile("test::mod", "extra.lisp", "(define extra 99)")

        let files = source.listFiles("test::mod")
        @Assert(files.contains("extra.lisp"), true)

        match (source.readFile("test::mod", "extra.lisp")) {
            case Some(content) =>
                @Assert(content, "(define extra 99)")
            case None =>
                @Assert(false, true)
        }
    }

    /**
     * 测试 MemorySource - 注册不同模块互不影响
     */
    @TestCase
    func testMemorySourceModuleIsolation() {
        let source = MemorySource()

        source.registerModule(
            "mod::a",
            "(module a ...)",
            HashMap<String, String>([("a.lisp", "(define a-val 1)")])
        )

        source.registerModule(
            "mod::b",
            "(module b ...)",
            HashMap<String, String>([("b.lisp", "(define b-val 2)")])
        )

        let aFiles = source.listFiles("mod::a")
        @Assert(aFiles.size == 1, true)
        @Assert(aFiles.contains("a.lisp"), true)
        @Assert(aFiles.contains("b.lisp"), false)

        let bFiles = source.listFiles("mod::b")
        @Assert(bFiles.size == 1, true)
        @Assert(bFiles.contains("b.lisp"), true)
        @Assert(bFiles.contains("a.lisp"), false)
    }

    /**
     * 测试 MemorySource - 空文件列表的模块
     */
    @TestCase
    func testMemorySourceEmptyModule() {
        let source = MemorySource()
        source.registerMetadata("empty::mod", "(module empty (version \"0.1\"))")

        let metadata = source.getMetadata("empty::mod")
        @Assert(metadata.isSome(), true)

        let files = source.listFiles("empty::mod")
        @Assert(files.isEmpty(), true)
    }

    /**
     * 测试 FileSystemSource - 现有模块
     */
    @TestCase
    func testFileSystemSourceExistingModule() {
        let searchPaths = ArrayList<String>()
        searchPaths.add("./lisp-tests")
        let source = FileSystemSource(searchPaths)

        let result = source.getMetadata("ystyle::log")
        if (let Some(m) <- result) {
            @Assert(m.contains("module"), true)
        }
    }

    /**
     * 测试 FileSystemSource - 不存在的模块
     */
    @TestCase
    func testFileSystemSourceNonexistentModule() {
        let source = FileSystemSource.fromPath(".")
        let metadata = source.getMetadata("nonexistent::module_xyz")
        @Assert(metadata.isNone(), true)
    }
}
