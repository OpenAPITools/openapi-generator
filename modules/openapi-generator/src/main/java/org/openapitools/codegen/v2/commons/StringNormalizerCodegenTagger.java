package org.openapitools.codegen.v2.commons;

import org.openapitools.codegen.v2.*;
import org.openapitools.codegen.v2.reflection.GenericClass;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.Collection;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

public final class StringNormalizerCodegenTagger implements CodegenTagger {

    private static final Logger LOGGER = LoggerFactory.getLogger(StringNormalizerCodegenTagger.class);

    private final Function<String, String> normalizeFunc;
    private final Collection<CodegenTag> tags;

    public StringNormalizerCodegenTagger(Function<String, String> standardizeFunc, CodegenTag... tags) {
        this(standardizeFunc, Arrays.asList(tags));
    }

    public StringNormalizerCodegenTagger(Function<String, String> normalizeFunc, Collection<CodegenTag> tags) {
        this.normalizeFunc = Objects.requireNonNull(normalizeFunc);
        this.tags = Objects.requireNonNull(tags);
    }

    @Override
    public void tag(CodegenObject object) {
        for (CodegenTag tag : tags) {
            if (tag.acceptsValueClass(String.class)) {
                String value = object.getTag(tag, String.class);
                if (value != null) {
                    object.setTag(tag, normalizeFunc.apply(value));
                }
            } else if (tag.acceptsValueGenericClass(new GenericClass<Collection<String>>() {})) {
                Collection<String> values = object.getTag(tag, new GenericClass<Collection<String>>() {});
                if (values != null) {
                    object.setTag(tag, values.stream().map(normalizeFunc).collect(Collectors.toList()));
                }
            }
        }
    }
}
