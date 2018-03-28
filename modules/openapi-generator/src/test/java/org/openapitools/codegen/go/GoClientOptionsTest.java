package org.openapitools.codegen.go;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.GoClientCodegen;
import org.openapitools.codegen.options.GoClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class GoClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private GoClientCodegen clientCodegen;

    public GoClientOptionsTest() {
        super(new GoClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setPackageVersion(GoClientOptionsProvider.PACKAGE_VERSION_VALUE);
            times = 1;
            clientCodegen.setPackageName(GoClientOptionsProvider.PACKAGE_NAME_VALUE);
            times = 1;
            clientCodegen.setWithXml(GoClientOptionsProvider.WITH_XML_VALUE);
            times = 1;
            clientCodegen.setPrependFormOrBodyParameters(Boolean.valueOf(GoClientOptionsProvider.PREPEND_FORM_OR_BODY_PARAMETERS_VALUE));
            times = 1;
        }};
    }
}
