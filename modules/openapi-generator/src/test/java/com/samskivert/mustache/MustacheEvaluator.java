package com.samskivert.mustache;

import java.io.StringReader;
import java.util.Map;

public class MustacheEvaluator {
    private final Template template;
    private final Template.Context context;

    private MustacheEvaluator(Template.Context context) {
        this.context = context;
        this.template = Mustache.compiler().compile(new StringReader(""));
    }

    public static MustacheEvaluator create(Map<String, Object> additionalProperties) {
        return new MustacheEvaluator(new MustacheTemplateContext(additionalProperties));
    }
    public Object getValue(String name) {
        return template.getValue(context, name, 0, false);
    }
}
