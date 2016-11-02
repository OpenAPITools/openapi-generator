package io.swagger.codegen.python;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.PythonClientCodegen;
import io.swagger.codegen.options.PythonClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class PythonClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private PythonClientCodegen clientCodegen;

    public PythonClientOptionsTest() {
        super(new PythonClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setPackageName(PythonClientOptionsProvider.PACKAGE_NAME_VALUE);
            times = 1;
            clientCodegen.setPackageVersion(PythonClientOptionsProvider.PACKAGE_VERSION_VALUE);
            times = 1;
            clientCodegen.setPackageUrl(PythonClientOptionsProvider.PACKAGE_URL_VALUE);
            times = 1;
        }};
    }
}
