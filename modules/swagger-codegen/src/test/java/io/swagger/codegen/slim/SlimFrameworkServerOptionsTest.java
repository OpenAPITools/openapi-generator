package io.swagger.codegen.slim;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.SlimFrameworkServerCodegen;
import io.swagger.codegen.options.SlimFrameworkServerOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class SlimFrameworkServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private SlimFrameworkServerCodegen clientCodegen;

    public SlimFrameworkServerOptionsTest() {
        super(new SlimFrameworkServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(SlimFrameworkServerOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
