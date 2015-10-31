package io.swagger.codegen.statichtml;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.StaticHtmlGenerator;
import io.swagger.codegen.options.StaticHtmlOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class StaticHtmlOptionsTest extends AbstractOptionsTest {

    @Tested
    private StaticHtmlGenerator clientCodegen;

    public StaticHtmlOptionsTest() {
        super(new StaticHtmlOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(StaticHtmlOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(StaticHtmlOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(StaticHtmlOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
