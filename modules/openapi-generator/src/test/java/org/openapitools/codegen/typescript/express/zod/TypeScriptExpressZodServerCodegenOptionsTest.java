package org.openapitools.codegen.typescript.express.zod;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.TypeScriptExpressZodServerCodegen;
import org.openapitools.codegen.options.TypeScriptExpressZodServerCodegenOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class TypeScriptExpressZodServerCodegenOptionsTest extends AbstractOptionsTest {
    private TypeScriptExpressZodServerCodegen codegen = mock(TypeScriptExpressZodServerCodegen.class, mockSettings);

    public TypeScriptExpressZodServerCodegenOptionsTest() {
        super(new TypeScriptExpressZodServerCodegenOptionsProvider());
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

