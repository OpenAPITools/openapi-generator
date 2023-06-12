package org.openapitools.codegen.n4js;

import static java.lang.Boolean.parseBoolean;
import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.openapitools.codegen.CodegenConstants.API_NAME_PREFIX;
import static org.openapitools.codegen.CodegenConstants.API_PACKAGE;
import static org.openapitools.codegen.CodegenConstants.MODEL_PACKAGE;
import static org.openapitools.codegen.languages.N4jsClientCodegen.CHECK_REQUIRED_PARAMS_NOT_NULL;
import static org.openapitools.codegen.languages.N4jsClientCodegen.CHECK_SUPERFLUOUS_BODY_PROPS;
import static org.openapitools.codegen.languages.N4jsClientCodegen.GENERATE_DEFAULT_API_EXECUTER;
import static org.openapitools.codegen.options.N4jsClientCodegenOptionsProvider.CHECK_REQUIRED_PARAMS_NOT_NULL__VALUE;
import static org.openapitools.codegen.options.N4jsClientCodegenOptionsProvider.CHECK_SUPERFLUOUS_BODY_PROPS__VALUE;
import static org.openapitools.codegen.options.N4jsClientCodegenOptionsProvider.GENERATE_DEFAULT_API_EXECUTER__VALUE;

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
        assertEquals(parseBoolean(CHECK_REQUIRED_PARAMS_NOT_NULL__VALUE),
                codegen.additionalProperties().get(CHECK_REQUIRED_PARAMS_NOT_NULL));
        assertEquals(parseBoolean(CHECK_SUPERFLUOUS_BODY_PROPS__VALUE),
                codegen.additionalProperties().get(CHECK_SUPERFLUOUS_BODY_PROPS));
        assertEquals(parseBoolean(GENERATE_DEFAULT_API_EXECUTER__VALUE),
                codegen.additionalProperties().get(GENERATE_DEFAULT_API_EXECUTER));

        assertEquals("", codegen.additionalProperties().get(API_PACKAGE));
        assertEquals("", codegen.additionalProperties().get(MODEL_PACKAGE));
        assertEquals("", codegen.additionalProperties().get(API_NAME_PREFIX));
    }
}
