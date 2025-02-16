package org.openapitools.codegen.dart.next;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.DartNextClientCodegen;
import org.openapitools.codegen.options.DartNextClientCodegenOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class DartNextClientCodegenOptionsTest extends AbstractOptionsTest {
    private DartNextClientCodegen codegen = mock(DartNextClientCodegen.class, mockSettings);

    public DartNextClientCodegenOptionsTest() {
        super(new DartNextClientCodegenOptionsProvider());
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

