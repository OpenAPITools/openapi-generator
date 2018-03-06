package org.openapitools.codegen.swagger;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.SwaggerGenerator;
import org.openapitools.codegen.options.SwaggerOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class SwaggerOptionsTest extends AbstractOptionsTest {

    @Tested
    private SwaggerGenerator clientCodegen;

    public SwaggerOptionsTest() {
        super(new SwaggerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(SwaggerOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
