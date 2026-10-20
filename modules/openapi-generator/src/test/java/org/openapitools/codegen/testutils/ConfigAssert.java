package org.openapitools.codegen.testutils;

import com.samskivert.mustache.MustacheEvaluator;
import org.junit.jupiter.api.Assertions;

import java.util.Map;
import java.util.function.Supplier;

/**
 * Utility class to assert variable values in mustache.
 *
 * @See {@link org.openapitools.codegen.DefaultCodegen#useCodegenAsMustacheParentContext() useCodegenAsMustacheParentContext}
 * <p>
 * The values come from additionalProperties or from codegen if useCodegenAsMustacheParentContext is called
 */
public class ConfigAssert {

    private final MustacheEvaluator evaluator;

    public ConfigAssert(Map<String, Object> additionalProperties) {
        this.evaluator = MustacheEvaluator.create(additionalProperties);
    }

    /**
     * Evaluate a mustache variable.
     *
     * @param name mustache variable
     * @return the value as seen by mustache as {{{name}}}
     */
    public Object getValue(String name) {
        return evaluator.getValue(name);
    }

    /**
     * Validate that property is correctly initialized for mustache.
     *
     * @param name          mustache variable
     * @param expectedValue value to match.
     * @throws AssertionError in case of mismatch
     */
    public void assertValue(String name, Object expectedValue) {
        Object actual = getValue(name);
        Assertions.assertEquals(expectedValue, actual, name + "not matching in mustache context");
    }

    /**
     * Validate that property is correctly initialized for mustache and in java.
     *
     * @param name          mustache variable
     * @param getter        value provider in java code (typically in DefaultCodegen or children of DefaultCodegen)
     * @param expectedValue value to match.
     * @throws AssertionError in case of mismatch
     */
    public void assertValue(String name, Supplier<Object> getter, Object expectedValue) {
        Object actual = getValue(name);
        Object codeGenExpectedValue = getter.get();
        Assertions.assertEquals(codeGenExpectedValue, actual, "valueNotFound in codegen");
        Assertions.assertEquals(expectedValue, actual, name + "not matching in mustache context");
    }
}
