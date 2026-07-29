package ystyle::xisp.repl

import std.unittest.*
import std.unittest.testmacro.*
import std.fs.*

@Test
class HistoryManagerTest {
    @TestCase
    func testAddEntry() {
        let hm = HistoryManager(maxEntries: 10)
        hm.add("first")
        hm.add("second")
        hm.add("third")
        @Assert(hm.getAll().size, 3)
        @Assert(hm.getAll()[0], "first")
        @Assert(hm.getAll()[1], "second")
        @Assert(hm.getAll()[2], "third")
    }

    @TestCase
    func testAddDuplicate() {
        let hm = HistoryManager(maxEntries: 10)
        hm.add("hello")
        hm.add("hello")
        @Assert(hm.getAll().size, 1)
        hm.add("world")
        hm.add("world")
        @Assert(hm.getAll().size, 2)
        hm.add("hello")
        @Assert(hm.getAll().size, 3)
        @Assert(hm.getAll()[0], "hello")
        @Assert(hm.getAll()[1], "world")
        @Assert(hm.getAll()[2], "hello")
    }

    @TestCase
    func testAddEmptyEntry() {
        let hm = HistoryManager(maxEntries: 10)
        hm.add("")
        @Assert(hm.getAll().size, 0)
        hm.add("   ")
        @Assert(hm.getAll().size, 0)
        hm.add("valid")
        @Assert(hm.getAll().size, 1)
    }

    @TestCase
    func testMaxEntries() {
        let hm = HistoryManager(maxEntries: 3)
        hm.add("a")
        hm.add("b")
        hm.add("c")
        @Assert(hm.getAll().size, 3)
        hm.add("d")
        @Assert(hm.getAll().size, 3)
        @Assert(hm.getAll()[0], "b")
        @Assert(hm.getAll()[1], "c")
        @Assert(hm.getAll()[2], "d")
        hm.add("e")
        @Assert(hm.getAll()[0], "c")
        @Assert(hm.getAll()[1], "d")
        @Assert(hm.getAll()[2], "e")
    }

    @TestCase
    func testMaxEntriesWithZero() {
        let hm = HistoryManager(maxEntries: 0)
        hm.add("a")
        @Assert(hm.getAll().size, 0)
    }

    @TestCase
    func testNavigateBackAndForth() {
        let hm = HistoryManager(maxEntries: 10)
        hm.add("a")
        hm.add("b")
        hm.add("c")
        hm.resetNavigation()

        match (hm.navigate(-1)) {
            case Some(v) => @Assert(v, "c")
            case None => @Fail("should navigate back to c")
        }
        match (hm.navigate(-1)) {
            case Some(v) => @Assert(v, "b")
            case None => @Fail("should navigate back to b")
        }
        match (hm.navigate(1)) {
            case Some(v) => @Assert(v, "c")
            case None => @Fail("should navigate forward to c")
        }
        match (hm.navigate(1)) {
            case Some(v) => @Fail("should be past end after forward from last")
            case None => ()
        }
    }

    @TestCase
    func testNavigateOutOfRange() {
        let hm = HistoryManager(maxEntries: 10)
        hm.add("a")
        hm.add("b")
        hm.resetNavigation()

        match (hm.navigate(-3)) {
            case Some(v) => @Fail("should be out of range going too far back")
            case None => ()
        }
        match (hm.navigate(1)) {
            case Some(v) => @Fail("should be out of range going past end")
            case None => ()
        }
    }

    @TestCase
    func testNavigateFromStart() {
        let hm = HistoryManager(maxEntries: 10)
        hm.add("a")
        hm.add("b")
        hm.resetNavigation()

        hm.navigate(-1)
        hm.navigate(-1)
        match (hm.navigate(-1)) {
            case Some(v) => @Fail("should be out of range at start")
            case None => ()
        }
    }

    @TestCase
    func testResetNavigation() {
        let hm = HistoryManager(maxEntries: 10)
        hm.add("a")
        hm.add("b")
        hm.add("c")

        hm.navigate(-1)
        hm.navigate(-1)
        @Assert(hm.getCurrentIndex(), 1)

        hm.resetNavigation()
        @Assert(hm.getCurrentIndex(), hm.getAll().size)
    }

    @TestCase
    func testSaveAndLoad() {
        let testPath = "/tmp/xisp_history_test_save_and_load.txt"
        try {
            if (exists(Path(testPath))) {
                remove(Path(testPath))
            }
        } catch (_: Error) {}

        let hm1 = HistoryManager(maxEntries: 10)
        hm1.add("line1")
        hm1.add("line2")
        hm1.add("line3")
        hm1.save(testPath)

        let hm2 = HistoryManager(maxEntries: 10)
        hm2.load(testPath)
        @Assert(hm2.getAll().size, 3)
        @Assert(hm2.getAll()[0], "line1")
        @Assert(hm2.getAll()[1], "line2")
        @Assert(hm2.getAll()[2], "line3")
        @Assert(hm2.getCurrentIndex(), 3)

        try {
            if (exists(Path(testPath))) {
                remove(Path(testPath))
            }
        } catch (_: Error) {}
    }

    @TestCase
    func testSaveAppendNewEntries() {
        let testPath = "/tmp/xisp_history_test_save_append.txt"
        try {
            if (exists(Path(testPath))) {
                remove(Path(testPath))
            }
        } catch (_: Error) {}

        let hm1 = HistoryManager(maxEntries: 10)
        hm1.add("a")
        hm1.add("b")
        hm1.save(testPath)

        hm1.add("c")
        hm1.add("d")
        hm1.save(testPath)

        let hm2 = HistoryManager(maxEntries: 10)
        hm2.load(testPath)
        @Assert(hm2.getAll().size, 4)
        @Assert(hm2.getAll()[0], "a")
        @Assert(hm2.getAll()[1], "b")
        @Assert(hm2.getAll()[2], "c")
        @Assert(hm2.getAll()[3], "d")

        try {
            if (exists(Path(testPath))) {
                remove(Path(testPath))
            }
        } catch (_: Error) {}
    }

    @TestCase
    func testLoadNonexistentFile() {
        let testPath = "/tmp/xisp_history_test_nonexistent.txt"
        try {
            if (exists(Path(testPath))) {
                remove(Path(testPath))
            }
        } catch (_: Error) {}

        let hm = HistoryManager(maxEntries: 10)
        hm.load(testPath)
        @Assert(hm.getAll().size, 0)
    }

    @TestCase
    func testLoadEmptyFile() {
        let testPath = "/tmp/xisp_history_test_empty.txt"
        try {
            if (exists(Path(testPath))) {
                remove(Path(testPath))
            }
            File.writeTo(testPath, Array<Byte>())
        } catch (_: Error) {}

        let hm = HistoryManager(maxEntries: 10)
        hm.load(testPath)
        @Assert(hm.getAll().size, 0)

        try {
            if (exists(Path(testPath))) {
                remove(Path(testPath))
            }
        } catch (_: Error) {}
    }

    @TestCase
    func testEmptyHistory() {
        let hm = HistoryManager(maxEntries: 10)
        @Assert(hm.getAll().size, 0)
        @Assert(hm.getCurrentIndex(), -1)

        match (hm.navigate(-1)) {
            case Some(_) => @Fail("empty history should return None for back")
            case None => ()
        }
        match (hm.navigate(1)) {
            case Some(_) => @Fail("empty history should return None for forward")
            case None => ()
        }
        hm.resetNavigation()
        @Assert(hm.getCurrentIndex(), 0)
    }
}
