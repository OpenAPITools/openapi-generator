package io.swagger.codegen.mustache;

import com.samskivert.mustache.Mustache;

import java.util.HashMap;
import java.util.Map;

public abstract class MustacheTestBase {
    protected Object context(Object... data) {
        Map<String, Object> ctx = new HashMap<>();
        if (data.length % 2 != 0) {
            throw new IllegalArgumentException("context helper accepts pairs of key/value varargs");
        }
        for (int i = 0; i < data.length; i += 2) {
            ctx.put(data[i].toString(), data[i + 1]);
        }
        return ctx;
    }
    protected String compile(String template, Object context) {
        return Mustache.compiler().compile(template).execute(context);
    }
}
