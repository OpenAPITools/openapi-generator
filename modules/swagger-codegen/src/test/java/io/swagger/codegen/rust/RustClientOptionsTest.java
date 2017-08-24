package io.swagger.codegen.rust;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.RustClientCodegen;
import io.swagger.codegen.options.RustClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class RustClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private RustClientCodegen clientCodegen;

    public RustClientOptionsTest() {
        super(new RustClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setPackageName(RustClientOptionsProvider.PACKAGE_NAME_VALUE);
            times = 1;
            clientCodegen.setPackageVersion(RustClientOptionsProvider.PACKAGE_VERSION_VALUE);
            times = 1;
        }};
    }
}
