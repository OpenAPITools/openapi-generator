package io.swagger.codegen.languages;

import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;

public class RustServerCodegenTest {

    @Test
    public void testRustIntSize() {
        assertEquals(RustServerCodegen.matchingIntType(true, null, null), "usize");
        assertEquals(RustServerCodegen.matchingIntType(true, 0L, null), "usize");
        assertEquals(RustServerCodegen.matchingIntType(true, 0xFFL, null), "usize");
        assertEquals(RustServerCodegen.matchingIntType(true, 0xFFFFL, null), "usize");
        assertEquals(RustServerCodegen.matchingIntType(true, 0x10000L, null), "u32");
        assertEquals(RustServerCodegen.matchingIntType(true, 0xFFFFFFFFL, null), "u32");
        assertEquals(RustServerCodegen.matchingIntType(true, 0x100000000L, null), "u64");
        assertEquals(RustServerCodegen.matchingIntType(true, null, 0xFFL), "u8");
        assertEquals(RustServerCodegen.matchingIntType(true, null, 0x100L), "u16");
        assertEquals(RustServerCodegen.matchingIntType(true, null, 0xFFFFL), "u16");
        assertEquals(RustServerCodegen.matchingIntType(true, null, 0x10000L), "u32");
        assertEquals(RustServerCodegen.matchingIntType(true, null, 0xFFFFFFFFL), "u32");
        assertEquals(RustServerCodegen.matchingIntType(true, null, 0x100000000L), "u64");
        assertEquals(RustServerCodegen.matchingIntType(true, 0xFFL, 0xFFL), "u8");
        assertEquals(RustServerCodegen.matchingIntType(true, 0x100L, 0x100L), "u16");
        assertEquals(RustServerCodegen.matchingIntType(true, 0xFFFFL, 0xFFFFL), "u16");
        assertEquals(RustServerCodegen.matchingIntType(true, 0x10000L, 0x10000L), "u32");
        assertEquals(RustServerCodegen.matchingIntType(true, 0xFFFFFFFFL, 0xFFFFFFFFL), "u32");
        assertEquals(RustServerCodegen.matchingIntType(true, 0x100000000L, 0x100000000L), "u64");

        assertEquals(RustServerCodegen.matchingIntType(false, null, null), "isize");
        assertEquals(RustServerCodegen.matchingIntType(false, -256L, null), "isize");
        assertEquals(RustServerCodegen.matchingIntType(false, -257L, null), "isize");
        assertEquals(RustServerCodegen.matchingIntType(false, -16385L, null), "isize");
        assertEquals(RustServerCodegen.matchingIntType(false, ((long) Short.MIN_VALUE) - 1, null), "i32");
        assertEquals(RustServerCodegen.matchingIntType(false, (long) Integer.MIN_VALUE, null), "i32");
        assertEquals(RustServerCodegen.matchingIntType(false, ((long) Integer.MIN_VALUE) - 1, null), "i64");
        assertEquals(RustServerCodegen.matchingIntType(false, Long.MIN_VALUE, null), "i64");
        assertEquals(RustServerCodegen.matchingIntType(false, null, 127L), "i8");
        assertEquals(RustServerCodegen.matchingIntType(false, null, 128L), "i16");
        assertEquals(RustServerCodegen.matchingIntType(false, null, (long) Short.MAX_VALUE), "i16");
        assertEquals(RustServerCodegen.matchingIntType(false, null, (long) Short.MAX_VALUE + 1), "i32");
        assertEquals(RustServerCodegen.matchingIntType(false, null, (long) Integer.MAX_VALUE), "i32");
        assertEquals(RustServerCodegen.matchingIntType(false, null, (long) Integer.MAX_VALUE + 1), "i64");
        assertEquals(RustServerCodegen.matchingIntType(false, null, Long.MAX_VALUE), "i64");
    }

}
