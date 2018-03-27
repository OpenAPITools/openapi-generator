package org.openapitools.codegen.silex;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.SilexServerCodegen;
import org.openapitools.codegen.options.SilexServerOptionsProvider;

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
