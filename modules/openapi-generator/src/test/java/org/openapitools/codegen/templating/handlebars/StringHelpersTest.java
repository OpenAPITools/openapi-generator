package org.openapitools.codegen.templating.handlebars;

import com.github.jknack.handlebars.Context;
import com.github.jknack.handlebars.Handlebars;
import com.github.jknack.handlebars.Template;
import com.github.jknack.handlebars.context.FieldValueResolver;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.IOException;
import java.util.HashMap;

import static org.testng.Assert.assertEquals;

public class StringHelpersTest {

    private Handlebars handlebars = null;

    private void evaluate(HashMap<String, Object> data, String template, String expect) throws IOException {

        Context context = Context
                .newBuilder(data)
                .resolver(
                        FieldValueResolver.INSTANCE)
                .build();

        Template tmpl = handlebars.compileInline(template);
        String actual = tmpl.apply(context);
        assertEquals(actual, expect);
    }

    @BeforeMethod
    public void setup() {
        handlebars = new Handlebars();
        handlebars.registerHelpers(StringHelpers.class);
    }
}
