package io.swagger.codegen.swift;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.SwiftCodegen;
import io.swagger.codegen.options.SwiftOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class SwiftOptionsTest extends AbstractOptionsTest {

    @Tested
    private SwiftCodegen clientCodegen;

    public SwiftOptionsTest() {
        super(new SwiftOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(SwiftOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setProjectName(SwiftOptionsProvider.PROJECT_NAME_VALUE);
            times = 1;
            clientCodegen.setResponseAs(SwiftOptionsProvider.RESPONSE_AS_VALUE.split(","));
            times = 1;
            clientCodegen.setUnwrapRequired(Boolean.valueOf(SwiftOptionsProvider.UNWRAP_REQUIRED_VALUE));
            times = 1;
        }};
    }
}
