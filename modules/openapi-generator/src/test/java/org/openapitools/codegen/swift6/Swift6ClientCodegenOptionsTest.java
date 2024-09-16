package org.openapitools.codegen.swift6;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.Swift6ClientCodegen;
import org.openapitools.codegen.options.Swift6ClientCodegenOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class Swift6ClientCodegenOptionsTest extends AbstractOptionsTest {
    private Swift6ClientCodegen codegen = mock(Swift6ClientCodegen.class, mockSettings);

    public Swift6ClientCodegenOptionsTest() {
        super(new Swift6ClientCodegenOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return codegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        // TODO: Complete options using Mockito
        // verify(codegen).someMethod(arguments)
    }
}

