package org.openapitools.codegen.kotlin.misk;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.KotlinMiskServerCodegen;
import org.openapitools.codegen.options.KotlinMiskServerCodegenOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class KotlinMiskServerCodegenOptionsTest extends AbstractOptionsTest {
    private KotlinMiskServerCodegen codegen = mock(KotlinMiskServerCodegen.class, mockSettings);

    public KotlinMiskServerCodegenOptionsTest() {
        super(new KotlinMiskServerCodegenOptionsProvider());
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

