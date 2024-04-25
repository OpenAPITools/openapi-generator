package org.openapitools.codegen.ruby;

import org.openapitools.codegen.languages.AbstractRubyCodegen;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

/**
 * Tests for AbstractRubyCodegen
 */
public class AbstractRubyCodegenTest {
    private AbstractRubyCodegen codegen;

    @BeforeMethod
    public void setup() {
        codegen = new AbstractRubyCodegen() {
        };
    }

    @Test
    public void testEscapeUnsafeCharacters() {
        Assert.assertEquals(codegen.escapeUnsafeCharacters("=begin"), "=_begin");
        Assert.assertEquals(codegen.escapeUnsafeCharacters("=end"), "=_end");
        Assert.assertEquals(codegen.escapeUnsafeCharacters("#{x}"), "\\#{x}");
    }
}
