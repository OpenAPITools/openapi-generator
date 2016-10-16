package io.swagger.codegen.aspnetcore;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.AspNetCoreServerCodegen;
import io.swagger.codegen.options.AspNetCoreServerOptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class AspNetCoreServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private AspNetCoreServerCodegen serverCodegen;

    public AspNetCoreServerOptionsTest() {
        super(new AspNetCoreServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return serverCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(serverCodegen) {{
            serverCodegen.setPackageName(AspNetCoreServerOptionsProvider.PACKAGE_NAME_VALUE);
            times = 1;
            serverCodegen.setPackageVersion(AspNetCoreServerOptionsProvider.PACKAGE_VERSION_VALUE);
            times = 1;
            serverCodegen.setSourceFolder(AspNetCoreServerOptionsProvider.SOURCE_FOLDER_VALUE);
            times = 1;
            serverCodegen.useDateTimeOffset(true);
            times = 1;
            serverCodegen.setUseCollection(false);
            times = 1;
            serverCodegen.setReturnICollection(false);
            times = 1;
        }};
    }
}
