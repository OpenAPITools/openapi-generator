package io.swagger.codegen.swift4;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.Swift4Codegen;
import io.swagger.codegen.options.Swift4OptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class Swift4OptionsTest extends AbstractOptionsTest {

    @Tested
    private Swift4Codegen clientCodegen;

    public Swift4OptionsTest() {
        super(new Swift4OptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(Swift4OptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setProjectName(Swift4OptionsProvider.PROJECT_NAME_VALUE);
            times = 1;
            clientCodegen.setResponseAs(Swift4OptionsProvider.RESPONSE_AS_VALUE.split(","));
            times = 1;
            clientCodegen.setUnwrapRequired(Boolean.valueOf(Swift4OptionsProvider.UNWRAP_REQUIRED_VALUE));
            times = 1;
            clientCodegen.setObjcCompatible(Boolean.valueOf(Swift4OptionsProvider.OBJC_COMPATIBLE_VALUE));
            times = 1;
            clientCodegen.setLenientTypeCast(Boolean.valueOf(Swift4OptionsProvider.LENIENT_TYPE_CAST_VALUE));
            times = 1;
        }};
    }
}
