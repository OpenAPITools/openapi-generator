package io.swagger.codegen.qtfivecpp;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.Qt5CPPGenerator;
import io.swagger.codegen.options.Qt5CPPOptionsProvider;

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

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(Qt5CPPOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(Qt5CPPOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(Qt5CPPOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
