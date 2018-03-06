package org.openapitools.codegen.qtfivecpp;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.Qt5CPPGenerator;
import org.openapitools.codegen.options.Qt5CPPOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class Qt5CPPOptionsTest extends AbstractOptionsTest {

    @Tested
    private Qt5CPPGenerator clientCodegen;

    public Qt5CPPOptionsTest() {
        super(new Qt5CPPOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(Qt5CPPOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
