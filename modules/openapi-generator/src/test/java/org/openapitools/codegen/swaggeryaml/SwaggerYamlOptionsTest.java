package org.openapitools.codegen.swaggeryaml;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.SwaggerYamlGenerator;
import org.openapitools.codegen.options.SwaggerYamlOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class SwaggerYamlOptionsTest extends AbstractOptionsTest {

    @Tested
    private SwaggerYamlGenerator clientCodegen;

    public SwaggerYamlOptionsTest() {
        super(new SwaggerYamlOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(SwaggerYamlOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
