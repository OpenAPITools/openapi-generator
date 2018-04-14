package org.openapitools.codegen.tizen;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.TizenClientCodegen;
import org.openapitools.codegen.options.TizenClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class TizenClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private TizenClientCodegen clientCodegen;

    public TizenClientOptionsTest() {
        super(new TizenClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(TizenClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
