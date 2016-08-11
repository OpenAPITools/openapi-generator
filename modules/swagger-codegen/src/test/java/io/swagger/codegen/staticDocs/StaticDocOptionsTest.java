package io.swagger.codegen.staticDocs;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.StaticDocCodegen;
import io.swagger.codegen.options.StaticDocOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class StaticDocOptionsTest extends AbstractOptionsTest {

    @Tested
    private StaticDocCodegen clientCodegen;

    public StaticDocOptionsTest() {
        super(new StaticDocOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(StaticDocOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
