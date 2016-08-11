package io.swagger.codegen.silex;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.SilexServerCodegen;
import io.swagger.codegen.options.SilexServerOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class SilexServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private SilexServerCodegen clientCodegen;

    public SilexServerOptionsTest() {
        super(new SilexServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(SilexServerOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
