package io.swagger.codegen.aspnet5;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.AspNet5ServerCodegen;
import io.swagger.codegen.languages.CSharpClientCodegen;
import io.swagger.codegen.options.AspNet5ServerOptionsProvider;
import io.swagger.codegen.options.CSharpClientOptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class AspNet5ServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private AspNet5ServerCodegen serverCodegen;

    public AspNet5ServerOptionsTest() {
        super(new AspNet5ServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return serverCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(serverCodegen) {{
            serverCodegen.setPackageName(AspNet5ServerOptionsProvider.PACKAGE_NAME_VALUE);
            times = 1;
            serverCodegen.setPackageVersion(AspNet5ServerOptionsProvider.PACKAGE_VERSION_VALUE);
            times = 1;
            serverCodegen.setSourceFolder(AspNet5ServerOptionsProvider.SOURCE_FOLDER_VALUE);
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
