package org.openapitools.codegen.statichtml;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.StaticHtmlGenerator;
import org.openapitools.codegen.options.StaticHtmlOptionsProvider;

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

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(StaticHtmlOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
