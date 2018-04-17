package org.openapitools.codegen.silex;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.PhpSilexServerCodegen;
import org.openapitools.codegen.options.PhpSilexServerOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class PhpSilexServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private PhpSilexServerCodegen clientCodegen;

    public PhpSilexServerOptionsTest() {
        super(new PhpSilexServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(PhpSilexServerOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
