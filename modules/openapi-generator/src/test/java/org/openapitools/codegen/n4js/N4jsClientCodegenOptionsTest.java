package org.openapitools.codegen.n4js;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.N4jsClientCodegen;
import org.openapitools.codegen.options.N4jsClientCodegenOptionsProvider;

public class N4jsClientCodegenOptionsTest extends AbstractOptionsTest {
    private N4jsClientCodegen codegen = mock(N4jsClientCodegen.class, mockSettings);

    public N4jsClientCodegenOptionsTest() {
        super(new N4jsClientCodegenOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return codegen;
    }

    @Override
    protected void verifyOptions() {
    	assertEquals(N4jsClientCodegenOptionsProvider.CHECK_REQUIRED_BODY_PROPS_NOT_NULL__VALUE, codegen.additionalProperties().get(N4jsClientCodegen.CHECK_REQUIRED_BODY_PROPS_NOT_NULL));
    	assertEquals(N4jsClientCodegenOptionsProvider.CHECK_SUPERFLUOUS_BODY_PROPS__VALUE, codegen.additionalProperties().get(N4jsClientCodegen.CHECK_SUPERFLUOUS_BODY_PROPS));
    }
}

