package io.swagger.codegen.tizen;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.TizenClientCodegen;
import io.swagger.codegen.options.TizenClientOptionsProvider;

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
