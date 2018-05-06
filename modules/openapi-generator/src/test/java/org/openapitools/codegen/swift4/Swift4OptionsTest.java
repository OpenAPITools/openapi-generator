package org.openapitools.codegen.swift4;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.Swift4Codegen;
import org.openapitools.codegen.options.Swift4OptionsProvider;
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
            clientCodegen.setPrependFormOrBodyParameters(Boolean.valueOf(Swift4OptionsProvider.PREPEND_FORM_OR_BODY_PARAMETERS_VALUE));
            times = 1;
        }};
    }
}
