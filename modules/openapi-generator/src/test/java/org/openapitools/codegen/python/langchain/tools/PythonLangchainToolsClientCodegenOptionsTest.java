package org.openapitools.codegen.python.langchain.tools;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.PythonLangchainToolsClientCodegen;
import org.openapitools.codegen.options.PythonLangchainToolsClientCodegenOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class PythonLangchainToolsClientCodegenOptionsTest extends AbstractOptionsTest {
    private PythonLangchainToolsClientCodegen codegen = mock(PythonLangchainToolsClientCodegen.class, mockSettings);

    public PythonLangchainToolsClientCodegenOptionsTest() {
        super(new PythonLangchainToolsClientCodegenOptionsProvider());
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

