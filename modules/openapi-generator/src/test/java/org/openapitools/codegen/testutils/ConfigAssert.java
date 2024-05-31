package org.openapitools.codegen.testutils;

import com.samskivert.mustache.MustacheEvaluator;
import org.junit.Assert;

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
        Assert.assertEquals("valueNotFound in  mustache context", expectedValue, actual);
    }

    public void assertValue(String name, Supplier<Object> getter, Object expectedValue) {
        Object actual = getValue(name);
        Object codeGenExpectedValue = getter.get();
        Assert.assertEquals("valueNotFound in codegen", codeGenExpectedValue, actual);
        Assert.assertEquals("valueNotFound in mustache context", expectedValue, actual);
    }
}
