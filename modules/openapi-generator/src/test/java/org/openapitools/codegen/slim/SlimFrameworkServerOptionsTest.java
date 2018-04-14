package org.openapitools.codegen.slim;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.SlimFrameworkServerCodegen;
import org.openapitools.codegen.options.SlimFrameworkServerOptionsProvider;

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
