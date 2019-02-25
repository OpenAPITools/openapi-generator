package org.openapitools.codegen;

import org.testng.Assert;
import org.testng.annotations.Test;

public class SpecValidationExceptionTest {

    @Test
    public void shouldGetDefaultMessage() {
        SpecValidationException specValidationException = new SpecValidationException();

        Assert.assertEquals(specValidationException.getMessage(), String.format("null | Error count: 0, Warning count: 0%sErrors: %s", System.lineSeparator(), System.lineSeparator()));
    }
}