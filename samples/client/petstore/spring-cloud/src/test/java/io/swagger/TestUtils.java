package io.swagger;

import java.util.Random;
import java.util.concurrent.atomic.AtomicLong;

public class TestUtils {
    private static final AtomicLong atomicId = createAtomicId();

    public static long nextId() {
        return atomicId.getAndIncrement();
    }

    private static AtomicLong createAtomicId() {
        int baseId = new Random(System.currentTimeMillis()).nextInt(1000000) + 20000;
        return new AtomicLong((long) baseId);
    }
}
