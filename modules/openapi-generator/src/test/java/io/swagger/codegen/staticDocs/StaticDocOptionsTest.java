package org.openapitools.codegen.staticDocs;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.StaticDocCodegen;
import org.openapitools.codegen.options.StaticDocOptionsProvider;

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
