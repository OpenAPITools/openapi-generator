/**
 * This is a performance benchmark that MUST be deleted before merging, as it is not intended to be part of the codebase.
 * It is only meant to be used for local testing and experimentation while the PR is on the fly.
 * The benchmark compares lookup approaches for small enums with 1 to 10 values, mirroring current enum generation.
 *
 * How to run this benchmark:
 * `./mvnw -f perf/jmh/pom.xml -q clean package && java -jar perf/jmh/target/benchmarks.jar SmallEnumLookupJmhBenchmark`
 * To save it to a CSV:
 * `./mvnw -f perf/jmh/pom.xml -q clean package && java -jar perf/jmh/target/benchmarks.jar SmallEnumLookupJmhBenchmark -rf csv -rff perf/jmh/target/perf-small-enum.csv`
 */
package org.openapitools.perf;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Level;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Param;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.infra.Blackhole;

@BenchmarkMode(Mode.Throughput)
@Warmup(iterations = 1, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(1)
@OutputTimeUnit(TimeUnit.SECONDS)
public class SmallEnumLookupJmhBenchmark {
    private enum Tiny1 {
        ONE("one");

        private static final Map<String, Tiny1> BY_VALUE_CS = new HashMap<>();
        private static final Map<String, Tiny1> BY_VALUE_CI = new HashMap<>();

        private final String value;

        static {
            for (Tiny1 e : values()) {
                if (!BY_VALUE_CS.containsKey(e.value)) {
                    BY_VALUE_CS.put(e.value, e);
                }
                String key = e.value == null ? null : e.value.toLowerCase(Locale.ROOT);
                if (!BY_VALUE_CI.containsKey(key)) {
                    BY_VALUE_CI.put(key, e);
                }
            }
        }

        Tiny1(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }

        public static Tiny1 fromValue(String value) {
            Tiny1 result = BY_VALUE_CS.get(value);
            if (result != null) {
                return result;
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny1 fromValueCaseInsensitive(String value) {
            Tiny1 result = BY_VALUE_CI.get(value.toLowerCase(Locale.ROOT));
            if (result != null) {
                return result;
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny1 fromValueLinear(String value) {
            for (Tiny1 b : Tiny1.values()) {
                if (b.value.equals(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny1 fromValueLinearCaseInsensitive(String value) {
            for (Tiny1 b : Tiny1.values()) {
                if (b.value.equalsIgnoreCase(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }
    }

    private enum Tiny2 {
        ONE("one"),
        TWO("two");

        private static final Map<String, Tiny2> BY_VALUE_CS = new HashMap<>();
        private static final Map<String, Tiny2> BY_VALUE_CI = new HashMap<>();

        private final String value;

        static {
            for (Tiny2 e : values()) {
                if (!BY_VALUE_CS.containsKey(e.value)) {
                    BY_VALUE_CS.put(e.value, e);
                }
                String key = e.value == null ? null : e.value.toLowerCase(Locale.ROOT);
                if (!BY_VALUE_CI.containsKey(key)) {
                    BY_VALUE_CI.put(key, e);
                }
            }
        }

        Tiny2(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }

        public static Tiny2 fromValue(String value) {
            Tiny2 result = BY_VALUE_CS.get(value);
            if (result != null) {
                return result;
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny2 fromValueCaseInsensitive(String value) {
            Tiny2 result = BY_VALUE_CI.get(value.toLowerCase(Locale.ROOT));
            if (result != null) {
                return result;
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny2 fromValueLinear(String value) {
            for (Tiny2 b : Tiny2.values()) {
                if (b.value.equals(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny2 fromValueLinearCaseInsensitive(String value) {
            for (Tiny2 b : Tiny2.values()) {
                if (b.value.equalsIgnoreCase(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }
    }

    private enum Tiny3 {
        ONE("one"),
        TWO("two"),
        THREE("three");

        private static final Map<String, Tiny3> BY_VALUE_CS = new HashMap<>();
        private static final Map<String, Tiny3> BY_VALUE_CI = new HashMap<>();

        private final String value;

        static {
            for (Tiny3 e : values()) {
                if (!BY_VALUE_CS.containsKey(e.value)) {
                    BY_VALUE_CS.put(e.value, e);
                }
                String key = e.value == null ? null : e.value.toLowerCase(Locale.ROOT);
                if (!BY_VALUE_CI.containsKey(key)) {
                    BY_VALUE_CI.put(key, e);
                }
            }
        }

        Tiny3(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }

        public static Tiny3 fromValue(String value) {
            Tiny3 result = BY_VALUE_CS.get(value);
            if (result != null) {
                return result;
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny3 fromValueCaseInsensitive(String value) {
            Tiny3 result = BY_VALUE_CI.get(value.toLowerCase(Locale.ROOT));
            if (result != null) {
                return result;
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny3 fromValueLinear(String value) {
            for (Tiny3 b : Tiny3.values()) {
                if (b.value.equals(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny3 fromValueLinearCaseInsensitive(String value) {
            for (Tiny3 b : Tiny3.values()) {
                if (b.value.equalsIgnoreCase(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }
    }

    private enum Tiny4 {
        ONE("one"),
        TWO("two"),
        THREE("three"),
        FOUR("four");

        private static final Map<String, Tiny4> BY_VALUE_CS = new HashMap<>();
        private static final Map<String, Tiny4> BY_VALUE_CI = new HashMap<>();

        private final String value;

        static {
            for (Tiny4 e : values()) {
                if (!BY_VALUE_CS.containsKey(e.value)) {
                    BY_VALUE_CS.put(e.value, e);
                }
                String key = e.value == null ? null : e.value.toLowerCase(Locale.ROOT);
                if (!BY_VALUE_CI.containsKey(key)) {
                    BY_VALUE_CI.put(key, e);
                }
            }
        }

        Tiny4(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }

        public static Tiny4 fromValue(String value) {
            Tiny4 result = BY_VALUE_CS.get(value);
            if (result != null) {
                return result;
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny4 fromValueCaseInsensitive(String value) {
            Tiny4 result = BY_VALUE_CI.get(value.toLowerCase(Locale.ROOT));
            if (result != null) {
                return result;
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny4 fromValueLinear(String value) {
            for (Tiny4 b : Tiny4.values()) {
                if (b.value.equals(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny4 fromValueLinearCaseInsensitive(String value) {
            for (Tiny4 b : Tiny4.values()) {
                if (b.value.equalsIgnoreCase(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }
    }

    private enum Tiny5 {
        ONE("one"),
        TWO("two"),
        THREE("three"),
        FOUR("four"),
        FIVE("five");

        private static final Map<String, Tiny5> BY_VALUE_CS = new HashMap<>();
        private static final Map<String, Tiny5> BY_VALUE_CI = new HashMap<>();

        private final String value;

        static {
            for (Tiny5 e : values()) {
                if (!BY_VALUE_CS.containsKey(e.value)) {
                    BY_VALUE_CS.put(e.value, e);
                }
                String key = e.value == null ? null : e.value.toLowerCase(Locale.ROOT);
                if (!BY_VALUE_CI.containsKey(key)) {
                    BY_VALUE_CI.put(key, e);
                }
            }
        }

        Tiny5(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }

        public static Tiny5 fromValue(String value) {
            Tiny5 result = BY_VALUE_CS.get(value);
            if (result != null) {
                return result;
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny5 fromValueCaseInsensitive(String value) {
            Tiny5 result = BY_VALUE_CI.get(value.toLowerCase(Locale.ROOT));
            if (result != null) {
                return result;
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny5 fromValueLinear(String value) {
            for (Tiny5 b : Tiny5.values()) {
                if (b.value.equals(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny5 fromValueLinearCaseInsensitive(String value) {
            for (Tiny5 b : Tiny5.values()) {
                if (b.value.equalsIgnoreCase(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }
    }

    private enum Tiny6 {
        ONE("one"),
        TWO("two"),
        THREE("three"),
        FOUR("four"),
        FIVE("five"),
        SIX("six");

        private static final Map<String, Tiny6> BY_VALUE_CS = new HashMap<>();
        private static final Map<String, Tiny6> BY_VALUE_CI = new HashMap<>();

        private final String value;

        static {
            for (Tiny6 e : values()) {
                if (!BY_VALUE_CS.containsKey(e.value)) {
                    BY_VALUE_CS.put(e.value, e);
                }
                String key = e.value == null ? null : e.value.toLowerCase(Locale.ROOT);
                if (!BY_VALUE_CI.containsKey(key)) {
                    BY_VALUE_CI.put(key, e);
                }
            }
        }

        Tiny6(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }

        public static Tiny6 fromValue(String value) {
            Tiny6 result = BY_VALUE_CS.get(value);
            if (result != null) {
                return result;
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny6 fromValueCaseInsensitive(String value) {
            Tiny6 result = BY_VALUE_CI.get(value.toLowerCase(Locale.ROOT));
            if (result != null) {
                return result;
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny6 fromValueLinear(String value) {
            for (Tiny6 b : Tiny6.values()) {
                if (b.value.equals(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny6 fromValueLinearCaseInsensitive(String value) {
            for (Tiny6 b : Tiny6.values()) {
                if (b.value.equalsIgnoreCase(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }
    }

    private enum Tiny7 {
        ONE("one"),
        TWO("two"),
        THREE("three"),
        FOUR("four"),
        FIVE("five"),
        SIX("six"),
        SEVEN("seven");

        private static final Map<String, Tiny7> BY_VALUE_CS = new HashMap<>();
        private static final Map<String, Tiny7> BY_VALUE_CI = new HashMap<>();

        private final String value;

        static {
            for (Tiny7 e : values()) {
                if (!BY_VALUE_CS.containsKey(e.value)) {
                    BY_VALUE_CS.put(e.value, e);
                }
                String key = e.value == null ? null : e.value.toLowerCase(Locale.ROOT);
                if (!BY_VALUE_CI.containsKey(key)) {
                    BY_VALUE_CI.put(key, e);
                }
            }
        }

        Tiny7(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }

        public static Tiny7 fromValue(String value) {
            Tiny7 result = BY_VALUE_CS.get(value);
            if (result != null) {
                return result;
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny7 fromValueCaseInsensitive(String value) {
            Tiny7 result = BY_VALUE_CI.get(value.toLowerCase(Locale.ROOT));
            if (result != null) {
                return result;
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny7 fromValueLinear(String value) {
            for (Tiny7 b : Tiny7.values()) {
                if (b.value.equals(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny7 fromValueLinearCaseInsensitive(String value) {
            for (Tiny7 b : Tiny7.values()) {
                if (b.value.equalsIgnoreCase(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }
    }

    private enum Tiny8 {
        ONE("one"),
        TWO("two"),
        THREE("three"),
        FOUR("four"),
        FIVE("five"),
        SIX("six"),
        SEVEN("seven"),
        EIGHT("eight");

        private static final Map<String, Tiny8> BY_VALUE_CS = new HashMap<>();
        private static final Map<String, Tiny8> BY_VALUE_CI = new HashMap<>();

        private final String value;

        static {
            for (Tiny8 e : values()) {
                if (!BY_VALUE_CS.containsKey(e.value)) {
                    BY_VALUE_CS.put(e.value, e);
                }
                String key = e.value == null ? null : e.value.toLowerCase(Locale.ROOT);
                if (!BY_VALUE_CI.containsKey(key)) {
                    BY_VALUE_CI.put(key, e);
                }
            }
        }

        Tiny8(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }

        public static Tiny8 fromValue(String value) {
            Tiny8 result = BY_VALUE_CS.get(value);
            if (result != null) {
                return result;
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny8 fromValueCaseInsensitive(String value) {
            Tiny8 result = BY_VALUE_CI.get(value.toLowerCase(Locale.ROOT));
            if (result != null) {
                return result;
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny8 fromValueLinear(String value) {
            for (Tiny8 b : Tiny8.values()) {
                if (b.value.equals(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny8 fromValueLinearCaseInsensitive(String value) {
            for (Tiny8 b : Tiny8.values()) {
                if (b.value.equalsIgnoreCase(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }
    }

    private enum Tiny9 {
        ONE("one"),
        TWO("two"),
        THREE("three"),
        FOUR("four"),
        FIVE("five"),
        SIX("six"),
        SEVEN("seven"),
        EIGHT("eight"),
        NINE("nine");

        private static final Map<String, Tiny9> BY_VALUE_CS = new HashMap<>();
        private static final Map<String, Tiny9> BY_VALUE_CI = new HashMap<>();

        private final String value;

        static {
            for (Tiny9 e : values()) {
                if (!BY_VALUE_CS.containsKey(e.value)) {
                    BY_VALUE_CS.put(e.value, e);
                }
                String key = e.value == null ? null : e.value.toLowerCase(Locale.ROOT);
                if (!BY_VALUE_CI.containsKey(key)) {
                    BY_VALUE_CI.put(key, e);
                }
            }
        }

        Tiny9(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }

        public static Tiny9 fromValue(String value) {
            Tiny9 result = BY_VALUE_CS.get(value);
            if (result != null) {
                return result;
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny9 fromValueCaseInsensitive(String value) {
            Tiny9 result = BY_VALUE_CI.get(value.toLowerCase(Locale.ROOT));
            if (result != null) {
                return result;
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny9 fromValueLinear(String value) {
            for (Tiny9 b : Tiny9.values()) {
                if (b.value.equals(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny9 fromValueLinearCaseInsensitive(String value) {
            for (Tiny9 b : Tiny9.values()) {
                if (b.value.equalsIgnoreCase(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }
    }

    private enum Tiny10 {
        ONE("one"),
        TWO("two"),
        THREE("three"),
        FOUR("four"),
        FIVE("five"),
        SIX("six"),
        SEVEN("seven"),
        EIGHT("eight"),
        NINE("nine"),
        TEN("ten");

        private static final Map<String, Tiny10> BY_VALUE_CS = new HashMap<>();
        private static final Map<String, Tiny10> BY_VALUE_CI = new HashMap<>();

        private final String value;

        static {
            for (Tiny10 e : values()) {
                if (!BY_VALUE_CS.containsKey(e.value)) {
                    BY_VALUE_CS.put(e.value, e);
                }
                String key = e.value == null ? null : e.value.toLowerCase(Locale.ROOT);
                if (!BY_VALUE_CI.containsKey(key)) {
                    BY_VALUE_CI.put(key, e);
                }
            }
        }

        Tiny10(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }

        public static Tiny10 fromValue(String value) {
            Tiny10 result = BY_VALUE_CS.get(value);
            if (result != null) {
                return result;
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny10 fromValueCaseInsensitive(String value) {
            Tiny10 result = BY_VALUE_CI.get(value.toLowerCase(Locale.ROOT));
            if (result != null) {
                return result;
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny10 fromValueLinear(String value) {
            for (Tiny10 b : Tiny10.values()) {
                if (b.value.equals(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Tiny10 fromValueLinearCaseInsensitive(String value) {
            for (Tiny10 b : Tiny10.values()) {
                if (b.value.equalsIgnoreCase(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }
    }

    @State(Scope.Thread)
    public static class LookupState {
        @Param({"1", "2", "3", "4", "5", "6", "7", "8", "9", "10"})
        public int enumSize;

        @Param({"50000"})
        public int inputSize;

        private String[] exactInputs;
        private String[] mixedCaseInputs;
        private int index;
        private LookupFunctions functions;

        @Setup(Level.Trial)
        public void setup() {
            functions = LookupFunctions.forSize(enumSize);
            exactInputs = buildInputs(inputSize, false, functions.values);
            mixedCaseInputs = buildInputs(inputSize, true, functions.values);
            index = 0;
        }

        public String nextExact() {
            return exactInputs[index++ % exactInputs.length];
        }

        public String nextMixedCase() {
            return mixedCaseInputs[index++ % mixedCaseInputs.length];
        }
    }

    private static final class LookupFunctions {
        private final String[] values;
        private final Function<String, Object> hashMap;
        private final Function<String, Object> hashMapCaseInsensitive;
        private final Function<String, Object> linear;
        private final Function<String, Object> linearCaseInsensitive;

        private LookupFunctions(
                String[] values,
                Function<String, Object> hashMap,
                Function<String, Object> hashMapCaseInsensitive,
                Function<String, Object> linear,
                Function<String, Object> linearCaseInsensitive) {
            this.values = values;
            // Methods
            this.hashMap = hashMap;
            this.hashMapCaseInsensitive = hashMapCaseInsensitive;
            this.linear = linear;
            this.linearCaseInsensitive = linearCaseInsensitive;
        }

        private static LookupFunctions forSize(int size) {
            switch (size) {
                case 1:
                    return new LookupFunctions(
                            enumValues(Tiny1.values()),
                            Tiny1::fromValue,
                            Tiny1::fromValueCaseInsensitive,
                            Tiny1::fromValueLinear,
                            Tiny1::fromValueLinearCaseInsensitive);
                case 2:
                    return new LookupFunctions(
                            enumValues(Tiny2.values()),
                            Tiny2::fromValue,
                            Tiny2::fromValueCaseInsensitive,
                            Tiny2::fromValueLinear,
                            Tiny2::fromValueLinearCaseInsensitive);
                case 3:
                    return new LookupFunctions(
                            enumValues(Tiny3.values()),
                            Tiny3::fromValue,
                            Tiny3::fromValueCaseInsensitive,
                            Tiny3::fromValueLinear,
                            Tiny3::fromValueLinearCaseInsensitive);
                case 4:
                    return new LookupFunctions(
                            enumValues(Tiny4.values()),
                            Tiny4::fromValue,
                            Tiny4::fromValueCaseInsensitive,
                            Tiny4::fromValueLinear,
                            Tiny4::fromValueLinearCaseInsensitive);
                case 5:
                    return new LookupFunctions(
                            enumValues(Tiny5.values()),
                            Tiny5::fromValue,
                            Tiny5::fromValueCaseInsensitive,
                            Tiny5::fromValueLinear,
                            Tiny5::fromValueLinearCaseInsensitive);
                case 6:
                    return new LookupFunctions(
                            enumValues(Tiny6.values()),
                            Tiny6::fromValue,
                            Tiny6::fromValueCaseInsensitive,
                            Tiny6::fromValueLinear,
                            Tiny6::fromValueLinearCaseInsensitive);
                case 7:
                    return new LookupFunctions(
                            enumValues(Tiny7.values()),
                            Tiny7::fromValue,
                            Tiny7::fromValueCaseInsensitive,
                            Tiny7::fromValueLinear,
                            Tiny7::fromValueLinearCaseInsensitive);
                case 8:
                    return new LookupFunctions(
                            enumValues(Tiny8.values()),
                            Tiny8::fromValue,
                            Tiny8::fromValueCaseInsensitive,
                            Tiny8::fromValueLinear,
                            Tiny8::fromValueLinearCaseInsensitive);
                case 9:
                    return new LookupFunctions(
                            enumValues(Tiny9.values()),
                            Tiny9::fromValue,
                            Tiny9::fromValueCaseInsensitive,
                            Tiny9::fromValueLinear,
                            Tiny9::fromValueLinearCaseInsensitive);
                case 10:
                    return new LookupFunctions(
                            enumValues(Tiny10.values()),
                            Tiny10::fromValue,
                            Tiny10::fromValueCaseInsensitive,
                            Tiny10::fromValueLinear,
                            Tiny10::fromValueLinearCaseInsensitive);
                default:
                    throw new IllegalArgumentException("Unsupported enum size: " + size);
            }
        }
    }

    @Benchmark
    public void hashMapLookup(LookupState state, Blackhole blackhole) {
        String value = state.nextExact();
        Object result = state.functions.hashMap.apply(value);
        blackhole.consume(result);
    }

    @Benchmark
    public void hashMapLookupCaseInsensitive(LookupState state, Blackhole blackhole) {
        String value = state.nextMixedCase();
        Object result = state.functions.hashMapCaseInsensitive.apply(value);
        blackhole.consume(result);
    }

    @Benchmark
    public void linearLookup(LookupState state, Blackhole blackhole) {
        String value = state.nextExact();
        Object result = state.functions.linear.apply(value);
        blackhole.consume(result);
    }

    @Benchmark
    public void linearLookupCaseInsensitive(LookupState state, Blackhole blackhole) {
        String value = state.nextMixedCase();
        Object result = state.functions.linearCaseInsensitive.apply(value);
        blackhole.consume(result);
    }

    private static String[] enumValues(Tiny1[] values) {
        String[] result = new String[values.length];
        for (int i = 0; i < values.length; i++) {
            result[i] = values[i].getValue();
        }
        return result;
    }

    private static String[] enumValues(Tiny2[] values) {
        String[] result = new String[values.length];
        for (int i = 0; i < values.length; i++) {
            result[i] = values[i].getValue();
        }
        return result;
    }

    private static String[] enumValues(Tiny3[] values) {
        String[] result = new String[values.length];
        for (int i = 0; i < values.length; i++) {
            result[i] = values[i].getValue();
        }
        return result;
    }

    private static String[] enumValues(Tiny4[] values) {
        String[] result = new String[values.length];
        for (int i = 0; i < values.length; i++) {
            result[i] = values[i].getValue();
        }
        return result;
    }

    private static String[] enumValues(Tiny5[] values) {
        String[] result = new String[values.length];
        for (int i = 0; i < values.length; i++) {
            result[i] = values[i].getValue();
        }
        return result;
    }

    private static String[] enumValues(Tiny6[] values) {
        String[] result = new String[values.length];
        for (int i = 0; i < values.length; i++) {
            result[i] = values[i].getValue();
        }
        return result;
    }

    private static String[] enumValues(Tiny7[] values) {
        String[] result = new String[values.length];
        for (int i = 0; i < values.length; i++) {
            result[i] = values[i].getValue();
        }
        return result;
    }

    private static String[] enumValues(Tiny8[] values) {
        String[] result = new String[values.length];
        for (int i = 0; i < values.length; i++) {
            result[i] = values[i].getValue();
        }
        return result;
    }

    private static String[] enumValues(Tiny9[] values) {
        String[] result = new String[values.length];
        for (int i = 0; i < values.length; i++) {
            result[i] = values[i].getValue();
        }
        return result;
    }

    private static String[] enumValues(Tiny10[] values) {
        String[] result = new String[values.length];
        for (int i = 0; i < values.length; i++) {
            result[i] = values[i].getValue();
        }
        return result;
    }

    private static String[] buildInputs(int size, boolean mixedCase, String[] values) {
        String[] result = new String[size];
        Random random = new Random(42);
        for (int i = 0; i < size; i++) {
            String value = values[random.nextInt(values.length)];
            if (mixedCase && random.nextBoolean()) {
                value = swapCase(value);
            }
            result[i] = value;
        }
        return result;
    }

    private static String swapCase(String value) {
        StringBuilder builder = new StringBuilder(value.length());
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            if (Character.isUpperCase(c)) {
                builder.append(Character.toLowerCase(c));
            } else if (Character.isLowerCase(c)) {
                builder.append(Character.toUpperCase(c));
            } else {
                builder.append(c);
            }
        }
        return builder.toString();
    }
}
