package io.swagger.codegen.slim;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.LumenServerCodegen;
import io.swagger.codegen.options.LumenServerOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class LumenServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private LumenServerCodegen clientCodegen;

    public LumenServerOptionsTest() {
        super(new LumenServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(LumenServerOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
