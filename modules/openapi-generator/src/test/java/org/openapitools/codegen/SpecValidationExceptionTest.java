package org.openapitools.codegen;

import org.testng.Assert;
import org.testng.annotations.Test;

public class SpecValidationExceptionTest {

    @Test
    public void shouldGetDefaultMessage() {
        SpecValidationException specValidationException = new SpecValidationException();

        String expectedResult = new StringBuffer("null | Error count: 0, Warning count: 0")
                .append(System.lineSeparator()).append("Errors: ")
                .append(System.lineSeparator()).toString();

        Assert.assertEquals(specValidationException.getMessage(), expectedResult);
    }
}