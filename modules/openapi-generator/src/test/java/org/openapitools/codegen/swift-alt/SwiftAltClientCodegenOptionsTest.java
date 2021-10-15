package org.openapitools.codegen.swift.alt;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.SwiftAltClientCodegen;
import org.openapitools.codegen.options.SwiftAltClientCodegenOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class SwiftAltClientCodegenOptionsTest extends AbstractOptionsTest {
    private SwiftAltClientCodegen codegen = mock(SwiftAltClientCodegen.class, mockSettings);

    public SwiftAltClientCodegenOptionsTest() {
        super(new SwiftAltClientCodegenOptionsProvider());
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

