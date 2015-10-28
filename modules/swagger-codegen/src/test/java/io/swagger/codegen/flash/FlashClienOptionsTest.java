package io.swagger.codegen.flash;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.FlashClientCodegen;

import com.google.common.collect.ImmutableMap;
import mockit.Expectations;
import mockit.Tested;

import java.util.Map;

public class FlashClienOptionsTest extends AbstractOptionsTest {
    protected static final String PACKAGE_NAME_VALUE = "io.swagger.flash";
    protected static final String PACKAGE_VERSION_VALUE = "1.0.0-SNAPSHOT";
    protected static final String INVOKER_PACKAGE_VALUE = "io.swagger.flash";
    protected static final String SOURCE_FOLDER_VALUE = "src/main/flex/test";

    @Tested
    private FlashClientCodegen clientCodegen;

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
            clientCodegen.setInvokerPackage(INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSourceFolder(SOURCE_FOLDER_VALUE);
            times = 1;
        }};
    }

    @Override
    protected Map<String, String> getAvaliableOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(CodegenConstants.PACKAGE_VERSION, PACKAGE_VERSION_VALUE)
                .put(CodegenConstants.INVOKER_PACKAGE, INVOKER_PACKAGE_VALUE)
                .put(CodegenConstants.SOURCE_FOLDER, SOURCE_FOLDER_VALUE)
                .build();
    }
}
