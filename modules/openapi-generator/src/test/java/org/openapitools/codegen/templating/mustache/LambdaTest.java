package org.openapitools.codegen.templating.mustache;

import static org.testng.Assert.assertEquals;

import java.util.HashMap;
import java.util.Map;

import com.samskivert.mustache.Mustache;

/**
 * Simple framework to test Mustache Lambdas. It avoids
 * <pre>compiler->compile->execute->assert</pre>
 * boilerplate code.
 *
 * Inspired by <a href="https://github.com/samskivert/jmustache/blob/master/src/test/java/com/samskivert/mustache/SharedTests.java">Jmustache SharedTests.java</a>
 *
 */
public abstract class LambdaTest {

    protected String execute(String template, Object ctx) {
        return execute(Mustache.compiler(), template, ctx);
    }

    protected String execute(Mustache.Compiler compiler, String template, Object ctx) {
        return compiler.compile(template).execute(ctx);
    }

    protected void test(String expected, String template, Map<String, Object> ctx) {
        test(Mustache.compiler(), expected, template, ctx);
    }

    protected void test(Mustache.Compiler compiler, String expected,String template, Map<String, Object> ctx) {
        assertEquals(execute(compiler, template, ctx), expected);
    }

    protected static Map<String, Object> context(Object... data) {
        Map<String, Object> ctx = new HashMap<String, Object>();
        for (int ii = 0; ii < data.length; ii += 2) {
            ctx.put(data[ii].toString(), data[ii + 1]);
        }
        return ctx;
    }

}
