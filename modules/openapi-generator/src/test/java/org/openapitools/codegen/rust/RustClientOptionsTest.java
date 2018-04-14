package org.openapitools.codegen.rust;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.RustClientCodegen;
import org.openapitools.codegen.options.RustClientOptionsProvider;

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
