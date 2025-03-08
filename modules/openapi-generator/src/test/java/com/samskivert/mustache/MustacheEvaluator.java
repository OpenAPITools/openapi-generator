package com.samskivert.mustache;

import java.io.StringReader;
import java.util.Map;

/**
 * Evaluate a mustache variable using the same mustache context as MustacheEngineAdapter.
 */
public class MustacheEvaluator {
    private final Template template;
    private final Template.Context context;

    private MustacheEvaluator(Template.Context context) {
        this.context = context;
        this.template = Mustache.compiler().compile(new StringReader(""));
    }

    /**
     * Create a mustache context from the additionalProperties.
     *
     * @param additionalProperties
     * @return a mustache evaluator with the context constructed as in MustacheEngineAdapter.
     */
    public static MustacheEvaluator create(Map<String, Object> additionalProperties) {
        return new MustacheEvaluator(new MustacheTemplateContext(additionalProperties));
    }

    /**
     * Compute the value from the mustache context.
     *
     * @param name variable name
     * @return the value as mustache would see
     */
    public Object getValue(String name) {
        return template.getValue(context, name, 0, false);
    }
}
