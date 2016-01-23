package io.swagger.codegen.java.jaxrs;

import io.swagger.codegen.CodegenParameter;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import org.apache.commons.io.IOUtils;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

public class AllowableValuesTest {

    private static final String TEMPLATE_FILE = "JavaJaxRS/jersey1_18/allowableValues.mustache";
    private static final String PROVIDER_NAME = "operations";

    private static String loadClassResource(Class<?> cls, String name) throws IOException {
        InputStream in = null;
        try {
            in = cls.getClassLoader().getResourceAsStream(name);
            return IOUtils.toString(in, StandardCharsets.UTF_8);
        } finally {
            IOUtils.closeQuietly(in);
        }
    }

    @DataProvider(name = PROVIDER_NAME)
    private static Object[][] resource() {
        final CodegenParameter param1 = new CodegenParameter();
        final CodegenParameter param2 = new CodegenParameter() {{
            allowableValues = ImmutableMap.<String, Object>of("values", ImmutableList.of("item1", "item2", "item3"));
        }};
        final CodegenParameter param3 = new CodegenParameter() {{
            allowableValues = ImmutableMap.<String, Object>of("min", 1, "max", 10);
        }};
        final CodegenParameter param4 = new CodegenParameter() {{
            allowableValues = ImmutableMap.<String, Object>of("min", 1);
        }};
        final CodegenParameter param5 = new CodegenParameter() {{
            allowableValues = ImmutableMap.<String, Object>of("max", 10);
        }};

        return new Object[][]{
                {param1, ""},
                {param2, "allowableValues=\"item1, item2, item3\""},
                {param3, "allowableValues=\"range=[1, 10]\""},
                {param4, "allowableValues=\"range=[1, infinity]\""},
                {param5, "allowableValues=\"range=[-infinity, 10]\""},
        };
    }

    @Test(dataProvider = PROVIDER_NAME)
    public void annotationsTest(CodegenParameter parameter, String expected) throws IOException {
        final Template template = Mustache.compiler().compile(loadClassResource(this.getClass(), TEMPLATE_FILE));

        Assert.assertEquals(template.execute(parameter), expected);
    }
}
