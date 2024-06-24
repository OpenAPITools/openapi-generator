package org.openapitools.server.api;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;

/**
 * Collectors with particular features useful from the generated code.
 */
public final class HCollectors {

    /**
     * Returns a new {@link java.util.stream.Collector} that applies a default value in the absence of provided values and returns
     * the resulting {@link java.util.List}.
     *
     * @param baseDefault default value expressed in the base type of the parameter
     * @param converter   converter which transforms the default value from the base type to the result type
     * @param <BT>        base type
     * @param <T>         result type
     * @return new collector
     */
    static <BT, T> Collector<T, ?, List<T>> toDefaultedList(BT baseDefault, Function<BT, T> converter) {
        return new DefaultableListCollector<>(baseDefault, converter);
    }

    static <T> Collector<T, ?, List<T>> toRequiredList(String paramName, ValidatorUtils.Validator validator) {
        return new RequiredListCollector(paramName, validator);
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

    private static class RequiredListCollector<T> extends AbstractListCollector<T> {

        private final ValidatorUtils.Validator validator;
        private final String paramName;

        private RequiredListCollector(String paramName, ValidatorUtils.Validator validator) {
            this.paramName = paramName;
            this.validator = validator;
        }

        @Override
        public Function<List<T>, List<T>> finisher() {
            return l -> validator.require(paramName, l);
        }
    }
}