package io.swagger.codegen.flash;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.FlashClientCodegen;
import io.swagger.codegen.options.FlashClienOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class FlashClienOptionsTest extends AbstractOptionsTest {

    @Tested
    private FlashClientCodegen clientCodegen;

    public FlashClienOptionsTest() {
        super(new FlashClienOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setPackageName(FlashClienOptionsProvider.PACKAGE_NAME_VALUE);
            times = 1;
            clientCodegen.setPackageVersion(FlashClienOptionsProvider.PACKAGE_VERSION_VALUE);
            times = 1;
            clientCodegen.setInvokerPackage(FlashClienOptionsProvider.INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSourceFolder(FlashClienOptionsProvider.SOURCE_FOLDER_VALUE);
            times = 1;
        }};
    }
}
