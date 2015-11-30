package io.swagger.codegen.csharp;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.CSharpClientCodegen;
import io.swagger.codegen.options.CSharpClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class CSharpClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private CSharpClientCodegen clientCodegen;

    public CSharpClientOptionsTest() {
        super(new CSharpClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setPackageName(CSharpClientOptionsProvider.PACKAGE_NAME_VALUE);
            times = 1;
            clientCodegen.setOptionalMethodArgumentFlag(true);
            times = 1;
            clientCodegen.setPackageVersion(CSharpClientOptionsProvider.PACKAGE_VERSION_VALUE);
            times = 1;
        }};
    }
}
