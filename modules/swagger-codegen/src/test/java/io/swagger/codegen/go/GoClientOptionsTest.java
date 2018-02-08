package io.swagger.codegen.go;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.GoClientCodegen;
import io.swagger.codegen.options.GoClientOptionsProvider;

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
        }};
    }
}
