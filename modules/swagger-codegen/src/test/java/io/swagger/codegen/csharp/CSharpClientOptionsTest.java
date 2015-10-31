package io.swagger.codegen.csharp;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.CSharpClientCodegen;

import com.google.common.collect.ImmutableMap;
import mockit.Expectations;
import mockit.Tested;

import java.util.Map;

public class CSharpClientOptionsTest extends AbstractOptionsTest {
    protected static final String PACKAGE_NAME_VALUE = "swagger_client_csharp";
    protected static final String PACKAGE_VERSION_VALUE = "1.0.0-SNAPSHOT";

    @Tested
    private CSharpClientCodegen clientCodegen;

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setPackageName(PACKAGE_NAME_VALUE);
            times = 1;
            clientCodegen.setPackageVersion(PACKAGE_VERSION_VALUE);
            times = 1;
        }};
    }

    @Override
    protected Map<String, String> getAvaliableOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(CodegenConstants.PACKAGE_VERSION, PACKAGE_VERSION_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, "true")
                .build();
    }
}
