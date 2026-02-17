/**
 * This is a performance benchmark that MUST be deleted before merging, as it is not intended to be part of the codebase.
 * It is only meant to be used for local testing and experimentation while the PR is on the fly.
 * The benchmark compares the performance of different approaches to look up enum values by their string representation, including:
 * - A HashMap-based lookup (case-sensitive)
 * - A TreeMap-based lookup (case-insensitive)
 * - A linear search (case-sensitive)
 * - A linear search (case-insensitive)
 * The benchmark generates random inputs based on a predefined set of fruit names and measures the average time
 * taken for each lookup method to find the corresponding enum value.
 * 
 * How to run this benchmark:
 * `./mvnw -f perf/jmh/pom.xml -q clean package && java -jar perf/jmh/target/benchmarks.jar EnumLookupJmhBenchmark`
 */
package org.openapitools.perf;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.TreeMap;
import java.util.Optional;
import java.util.Locale;
import java.util.concurrent.TimeUnit;
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
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
public class EnumLookupJmhBenchmark {
    private enum Fruit {
        APPLE("apple"),
        BANANA("banana"),
        CHERRY("cherry"),
        DATE("date"),
        ELDERBERRY("elderberry"),
        FIG("fig"),
        GRAPE("grape"),
        HONEYDEW("honeydew"),
        KIWI("kiwi"),
        LEMON("lemon"),
        MANGO("mango"),
        NECTARINE("nectarine"),
        ORANGE("orange"),
        PAPAYA("papaya"),
        QUINCE("quince"),
        RASPBERRY("raspberry"),
        STRAWBERRY("strawberry"),
        TANGERINE("tangerine"),
        UGLI("ugli"),
        WATERMELON("watermelon"),
        AVOCADO("avocado"),
        BLACKBERRY("blackberry"),
        CANTALOUPE("cantaloupe"),
        DRAGONFRUIT("dragonfruit"),
        GRAPEFRUIT("grapefruit"),
        HUCKLEBERRY("huckleberry"),
        JACKFRUIT("jackfruit"),
        LIME("lime"),
        PAPAW("papaw"),
        PEACH("peach"),
        PEAR("pear"),
        PLUM("plum"),
        POMEGRANATE("pomegranate"),
        STARFRUIT("starfruit"),
        TOMATO("tomato"),  // yes, it's a fruit!
        YUZU("yuzu");
        
        private static final Map<String, Fruit> BY_VALUE_HASHMAP = new HashMap<>();
        private static final Map<String, Fruit> BY_VALUE_TREEMAP = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);

        private final String value;

        static {
            for (Fruit fruit : values()) {
                BY_VALUE_HASHMAP.put(fruit.value, fruit);
                BY_VALUE_TREEMAP.put(fruit.value, fruit);
            }
        }

        Fruit(String value) {
            this.value = value;
        }

        public static Fruit fromValueHashMapWithoutOptional(String value) {
            Fruit result = BY_VALUE_HASHMAP.get(value);
            if (result == null) {
                throw new IllegalArgumentException("Unexpected value '" + value + "'");
            }
            return result;
        }

        public static Fruit fromValueHashMapCaseInsensitiveWithOptional(String value) {
            return Optional.ofNullable(value == null ? null : BY_VALUE_HASHMAP.get(value.toLowerCase(Locale.ROOT))).orElseThrow(() -> new IllegalArgumentException("Unexpected value '" + value + "'"));
        }

        public static Fruit fromValueHashMapWithOptional(String value) {
            return Optional.ofNullable(BY_VALUE_HASHMAP.get(value)).orElseThrow(() -> new IllegalArgumentException("Unexpected value '" + value + "'"));
        }

        public static Fruit fromValueTreeMapWithOptional(String value) {
            return Optional.ofNullable(BY_VALUE_TREEMAP.get(value)).orElseThrow(() -> new IllegalArgumentException("Unexpected value '" + value + "'"));
        }

        public static Fruit fromValueLinear(String value) {
            for (Fruit b : Fruit.values()) {
                if (b.value.equals(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }

        public static Fruit fromValueLinearCaseInsensitive(String value) {
            for (Fruit b : Fruit.values()) {
                if (b.value.equalsIgnoreCase(value)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + value + "'");
        }
    }

    @State(Scope.Thread)
    public static class LookupState {
        @Param({"50000"})
        public int inputSize;

        private String[] exactInputs;
        private String[] mixedCaseInputs;
        private int index;

        @Setup(Level.Trial)
        public void setup() {
            exactInputs = buildInputs(inputSize, false);
            mixedCaseInputs = buildInputs(inputSize, true);
            index = 0;
        }

        public String nextExact() {
            String value = exactInputs[index++ % exactInputs.length];
            return value;
        }

        public String nextMixedCase() {
            String value = mixedCaseInputs[index++ % mixedCaseInputs.length];
            return value;
        }
    }
    
    @Benchmark
    public void hashMapLookupWithoutOptional(LookupState state, Blackhole blackhole) {
        String value = state.nextExact();
        Fruit result = Fruit.fromValueHashMapWithoutOptional(value);
        blackhole.consume(result);
    }
    
    @Benchmark
    public void hashMapLookupCaseInsensitive(LookupState state, Blackhole blackhole) {
        String value = state.nextMixedCase();
        Fruit result = Fruit.fromValueHashMapCaseInsensitiveWithOptional(value);
        blackhole.consume(result);
    }

    @Benchmark
    public void hashMapLookupWithOptional(LookupState state, Blackhole blackhole) {
        String value = state.nextExact();
        Fruit result = Fruit.fromValueHashMapWithOptional(value);
        blackhole.consume(result);
    }

    @Benchmark
    public void linearLookup(LookupState state, Blackhole blackhole) {
        String value = state.nextExact();
        Fruit result = Fruit.fromValueLinear(value);
        blackhole.consume(result);
    }

    @Benchmark
    public void linearLookupCaseInsensitive(LookupState state, Blackhole blackhole) {
        String value = state.nextMixedCase();
        Fruit result = Fruit.fromValueLinearCaseInsensitive(value);
        blackhole.consume(result);
    }


    @Benchmark
    public void treeMapLookupCaseInsensitiveWithOptional(LookupState state, Blackhole blackhole) {
        String value = state.nextMixedCase();
        Fruit result = Fruit.fromValueTreeMapWithOptional(value);
        blackhole.consume(result);
    }


    private static String[] buildInputs(int size, boolean mixedCase) {
        String[] values = new String[size];
        Fruit[] fruits = Fruit.values();
        Random random = new Random(42);  // The meaning of life, the universe, and everything
        for (int i = 0; i < size; i++) {
            String value = fruits[random.nextInt(fruits.length)].value;
            if (mixedCase && random.nextBoolean()) {
                value = swapCase(value);
            }
            values[i] = value;
        }
        return values;
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
