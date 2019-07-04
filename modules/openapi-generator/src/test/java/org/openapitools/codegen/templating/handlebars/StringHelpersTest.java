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

    @Test(description = "Handlebars StringHelpers.startsWith, section")
    public void startsWithSectionalTest() throws IOException {
        HashMap<String, Object> data = new HashMap<String, Object>() {{
            put("asdf", "asdf");
            put("a", "a");
            put("b", "b");
        }};

        String template = "{{~#startsWith asdf a ~}}yes{{~else~}}no{{~/startsWith~}}";
        evaluate(data, template, "yes");

        template = "{{~#startsWith asdf b ~}}yes{{~else~}}no{{~/startsWith~}}";
        evaluate(data, template, "no");
    }

    @Test(description = "Handlebars StringHelpers.startsWith")
    public void startsWithTest() throws IOException {
        HashMap<String, Object> data = new HashMap<String, Object>() {{
            put("asdf", "asdf");
            put("ASDF", "ASDF");
            put("a", "a");
            put("b", "b");
        }};
        evaluate(data, "{{startsWith asdf a}}", "true");
        evaluate(data, "{{startsWith asdf b}}", "false");
        evaluate(data, "{{startsWith ASDF a insensitive=true }}", "true");
        evaluate(data, "{{startsWith ASDF a insensitive=false }}", "false");
        evaluate(data, "{{startsWith ASDF 'a' insensitive=true }}", "true");
        evaluate(data, "{{startsWith ASDF b insensitive=true }}", "false");
        evaluate(data, "{{startsWith ASDF 'b' insensitive=true }}", "false");
        evaluate(data, "{{startsWith ASDF insensitive=true text='a'}}", "true");
        evaluate(data, "{{startsWith ASDF insensitive=true text='a' yes='✓' no='✘'}}", "✓");
        evaluate(data, "{{startsWith ASDF insensitive=false text='a' yes='✓' no='✘'}}", "✘");
    }

    @Test(description = "Handlebars StringHelpers.startsWith, yes/no override")
    public void startsWithYesOverrideTest() throws IOException {
        HashMap<String, Object> data = new HashMap<String, Object>() {{
            put("asdf", "asdf");
            put("a", "a");
            put("b", "b");
        }};
        String template = "{{startsWith asdf a yes='y' no='n'}}";
        evaluate(data, template, "y");

        template = "{{startsWith asdf b yes='y' no='n'}}";
        evaluate(data, template, "n");
    }
}