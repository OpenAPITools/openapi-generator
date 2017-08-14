package io.swagger.codegen.languages;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;
import org.testng.Assert;

public class AbstractScalaCodegenTest {

    private AbstractScalaCodegen abstractScalaCodegen;

    @BeforeTest
    public void setup() {
        this.abstractScalaCodegen = new FakeScalaCodeGen();
    }

    @Test
    public void shouldCamelCase() {
        String className = "models.WebsiteBodyModel";

        String result = abstractScalaCodegen.formatIdentifier(className, false);

        Assert.assertTrue("modelsWebsiteBodyModel".equals(result));
    }

    @Test
    public void shouldCamelCaseAndUpperCase() {
        String className = "models.WebsiteBodyModel";

        String result = abstractScalaCodegen.formatIdentifier(className, true);

        Assert.assertTrue("ModelsWebsiteBodyModel".equals(result));
    }

    @Test
    public void shouldEscapeReservedWords() {
        String className = "ReservedWord";

        String result = abstractScalaCodegen.formatIdentifier(className, true);

        Assert.assertTrue("_ReservedWord".equals(result));
    }

    @Test
    public void shouldReturnSameInputWhenNull() {
        String result = abstractScalaCodegen.stripPackageName(null);

        Assert.assertNull(result);
    }

    @Test
    public void shouldReturnSameInputWhenEmpty() {
        String input = "";
        String result = abstractScalaCodegen.stripPackageName(input);

        Assert.assertSame(result, input);
    }

    @Test
    public void shouldReturnSameInputWhenValid() {
        String input = "WebsiteBodyModel";
        String result = abstractScalaCodegen.stripPackageName(input);

        Assert.assertSame(result, input);
    }

    @Test
    public void shouldStripPackageName() {
        String input = "models.WebsiteBodyModel";
        String result = abstractScalaCodegen.stripPackageName(input);

        Assert.assertEquals(result, "WebsiteBodyModel");
    }

    private class FakeScalaCodeGen extends AbstractScalaCodegen {
        public FakeScalaCodeGen() {
            super();
            this.reservedWords.add("reservedword");
        }
    }
}