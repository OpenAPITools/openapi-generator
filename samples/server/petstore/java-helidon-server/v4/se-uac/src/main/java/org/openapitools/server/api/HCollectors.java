package org.openapitools.server.api;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;

import io.helidon.common.GenericType;
import io.helidon.http.Headers;
import io.helidon.http.WritableHeaders;
import io.helidon.http.media.EntityReader;
import io.helidon.http.media.FormParamsSupport;
import io.helidon.http.media.MediaSupport;

/**
 * Collectors with particular features useful from the generated code.
 */
@io.helidon.common.Generated(value = "org.openapitools.codegen.languages.JavaHelidonServerCodegen",
                             trigger = "utility",
                             version = "stable")
public final class HCollectors {

    private static final MediaSupport FORM_PARAMS_SUPPORT = FormParamsSupport.create();
    private static final Headers EMPTY_HEADERS = WritableHeaders.create();

    /**
     * Returns a new {@link java.util.stream.Collector} that applies a default value in the absence of provided values and returns
     * the resulting {@link java.util.List}.
     *
     * @param baseDefault default value expressed in the base type of the parameter
     * @param converter   transforms the default value from the base type to the result type
     * @param <BT>        base type
     * @param <T>         result type
     * @return new collector
     */
    static <BT, T> Collector<T, ?, List<T>> toDefaultedList(BT baseDefault, Function<BT, T> converter) {
        return new DefaultableListCollector<>(baseDefault, converter);
    }

    static <T> Collector<String, ?, Map<String, T>> noOpMap() {

        return new NoOpMapCollector<T>();
    }

    /**
     * Decodes URL encoded string as binary data.
     * <p>
     *     Inspired hugely by java.net.URLDecoder.
     * </p>
     *
     * @param s URL encoded string
     * @return binary data encoded by the string
     */
    static byte[] decodeBinaryFormParam(String s) {
        boolean needToChange = false;
        int numChars = s.length();
        int i = 0;

        char c;
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        byte[] bytes = null;
        while (i < numChars) {
            c = s.charAt(i);
            switch (c) {
            case '+':
                output.write(' ');
                i++;
                needToChange = true;
                break;
            case '%':
                /*
                 * Starting with this instance of %, process all
                 * consecutive substrings of the form %xy. Each
                 * substring %xy will yield a byte. Convert all
                 * consecutive  bytes obtained this way to whatever
                 * character(s) they represent in the provided
                 * encoding.
                 */

                try {

                    // (numChars-i)/3 is an upper bound for the number
                    // of remaining bytes
                    if (bytes == null)
                        bytes = new byte[(numChars-i)/3];
                    int pos = 0;

                    while ( ((i+2) < numChars) &&
                            (c=='%')) {
                        int v = Integer.parseInt(s, i + 1, i + 3, 16);
                        if (v < 0)
                            throw new IllegalArgumentException(
                                    "URLDecoder: Illegal hex characters in escape "
                                            + "(%) pattern - negative value");
                        bytes[pos++] = (byte) v;
                        i+= 3;
                        if (i < numChars)
                            c = s.charAt(i);
                    }

                    // A trailing, incomplete byte encoding such as
                    // "%x" will cause an exception to be thrown

                    if ((i < numChars) && (c=='%'))
                        throw new IllegalArgumentException(
                                "URLDecoder: Incomplete trailing escape (%) pattern");

                    output.write(bytes, 0, pos);
                } catch (NumberFormatException e) {
                    throw new IllegalArgumentException(
                            "URLDecoder: Illegal hex characters in escape (%) pattern - "
                                    + e.getMessage());
                }
                needToChange = true;
                break;
            default:
                output.write(c);
                i++;
                break;
            }
        }

        return output.toByteArray();
    }

    private static class NoOpMapCollector<T> implements Collector<String, Map<String, T>, Map<String, T>> {

        @Override
        public Supplier<Map<String, T>> supplier() {
            return LinkedHashMap::new;
        }

        @Override
        public BiConsumer<Map<String, T>, String> accumulator() {
            return (map, expr) -> {};
            }

        @Override
        public BinaryOperator<Map<String, T>> combiner() {
            return (a, b) -> a;
        }

        @Override
        public Function<Map<String, T>, Map<String, T>> finisher() {
            return Function.identity();
        }

        @Override
        public Set<Characteristics> characteristics() {
            return Set.of();
        }
    }

    private abstract static class AbstractListCollector<T> implements Collector<T, List<T>, List<T>> {

        @Override
        public Supplier<List<T>> supplier() {
            return ArrayList::new;
        }

        @Override
        public BiConsumer<List<T>, T> accumulator() {
            return List::add;
        }

        @Override
        public BinaryOperator<List<T>> combiner() {
            return (l1, l2) -> {
                l2.addAll(l1);
                return l2;
            };
        }

        @Override
        public Set<Characteristics> characteristics() {
            return Set.of();
        }
    }

    /**
     * {@link java.util.stream.Collector} for streams of the base type (values from the request, often strings}
     * applying a converter to the desired result type and supplying a default value if there are no base values.
     *
     * @param <BT> base type (often string)
     * @param <T>  result type
     */
    private static class DefaultableListCollector<BT, T> extends AbstractListCollector<T> {

        private final BT baseDefault;
        private final Function<BT, T> converter;

        DefaultableListCollector(BT baseDefault, Function<BT, T> converter) {
            this.baseDefault = baseDefault;
            this.converter = converter;
        }

        @Override
        public Function<List<T>, List<T>> finisher() {
            return l -> {
                if (l.isEmpty()) {
                    l.add(converter.apply(baseDefault));
                }
                return l;
            };
        }
    }
}