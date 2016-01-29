package io.swagger.codegen.swaggeryaml;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.SwaggerYamlGenerator;
import io.swagger.codegen.options.SwaggerYamlOptionsProvider;

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
