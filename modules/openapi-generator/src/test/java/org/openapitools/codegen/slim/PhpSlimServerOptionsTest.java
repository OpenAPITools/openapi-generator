package org.openapitools.codegen.slim;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.PhpSlimServerCodegen;
import org.openapitools.codegen.options.PhpSlimServerOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class PhpSlimServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private PhpSlimServerCodegen clientCodegen;

    public PhpSlimServerOptionsTest() {
        super(new PhpSlimServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(PhpSlimServerOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
