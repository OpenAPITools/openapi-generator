package org.openapitools.codegen.python;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.PythonClientCodegen;
import org.openapitools.codegen.options.PythonClientOptionsProvider;

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
            clientCodegen.setProjectName(PythonClientOptionsProvider.PROJECT_NAME_VALUE);
            clientCodegen.setPackageVersion(PythonClientOptionsProvider.PACKAGE_VERSION_VALUE);
            clientCodegen.setPackageUrl(PythonClientOptionsProvider.PACKAGE_URL_VALUE);
            // clientCodegen.setLibrary(PythonClientCodegen.DEFAULT_LIBRARY);
            times = 1;
        }};
    }
}
