package io.swagger.codegen.swagger;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.SwaggerGenerator;
import io.swagger.codegen.options.SwaggerOptionsProvider;

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

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(SwaggerOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(SwaggerOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(SwaggerOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
