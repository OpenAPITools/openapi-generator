package org.openapitools.codegen.typescript.express.zod;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.TypescriptExpressZodServerCodegen;
import org.openapitools.codegen.options.TypescriptExpressZodServerCodegenOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class TypescriptExpressZodServerCodegenOptionsTest extends AbstractOptionsTest {
    private TypescriptExpressZodServerCodegen codegen = mock(TypescriptExpressZodServerCodegen.class, mockSettings);

    public TypescriptExpressZodServerCodegenOptionsTest() {
        super(new TypescriptExpressZodServerCodegenOptionsProvider());
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

