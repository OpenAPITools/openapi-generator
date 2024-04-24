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
        Assertions.assertEquals(codegen.escapeUnsafeCharacters("=begin"), "=_begin");
        Assertions.assertEquals(codegen.escapeUnsafeCharacters("=end"), "=_end");
        Assertions.assertEquals(codegen.escapeUnsafeCharacters("#{x}"), "\\#{x}");
    }
}
