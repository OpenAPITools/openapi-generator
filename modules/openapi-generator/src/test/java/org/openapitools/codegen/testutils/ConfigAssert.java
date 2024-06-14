package org.openapitools.codegen.testutils;

import com.samskivert.mustache.MustacheEvaluator;
import org.junit.jupiter.api.Assertions;


import java.util.Map;
import java.util.function.Supplier;

/**
 * Utility class to evaluate values like mustache would do.
 * <p>
 * To avoid replicating a lot of code, reflection is used on protected classes
 */
public class ConfigAssert {

    private final MustacheEvaluator evaluator;


    public ConfigAssert(Map<String, Object> additionalProperties) {
        this.evaluator = MustacheEvaluator.create(additionalProperties);
    }

    public Object getValue(String name) {
        return evaluator.getValue(name);
    }

    public void assertValue(String name, Object expectedValue) {
        Object actual = getValue(name);
        Assertions.assertEquals(expectedValue, actual, "valueNotFound in  mustache context");
    }

    public void assertValue(String name, Supplier<Object> getter, Object expectedValue) {
        Object actual = getValue(name);
        Object codeGenExpectedValue = getter.get();
        Assertions.assertEquals(codeGenExpectedValue, actual, "valueNotFound in codegen");
        Assertions.assertEquals(expectedValue, actual, "valueNotFound in mustache context");
    }
}
