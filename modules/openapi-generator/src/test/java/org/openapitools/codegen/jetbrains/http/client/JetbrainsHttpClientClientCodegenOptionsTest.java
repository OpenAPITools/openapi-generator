package org.openapitools.codegen.jetbrains.http.client;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.JetbrainsHttpClientClientCodegen;
import org.openapitools.codegen.options.JetbrainsHttpClientClientCodegenOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class JetbrainsHttpClientClientCodegenOptionsTest extends AbstractOptionsTest {
    private JetbrainsHttpClientClientCodegen codegen = mock(JetbrainsHttpClientClientCodegen.class, mockSettings);

    public JetbrainsHttpClientClientCodegenOptionsTest() {
        super(new JetbrainsHttpClientClientCodegenOptionsProvider());
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

