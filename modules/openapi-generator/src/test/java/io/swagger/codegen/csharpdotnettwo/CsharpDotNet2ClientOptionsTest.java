package org.openapitools.codegen.csharpdotnettwo;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.CsharpDotNet2ClientCodegen;
import org.openapitools.codegen.options.CsharpDotNet2ClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class CsharpDotNet2ClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private CsharpDotNet2ClientCodegen clientCodegen;

    public CsharpDotNet2ClientOptionsTest() {
        super(new CsharpDotNet2ClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setPackageName(CsharpDotNet2ClientOptionsProvider.PACKAGE_NAME_VALUE);
            times = 1;
            clientCodegen.setPackageVersion(CsharpDotNet2ClientOptionsProvider.PACKAGE_VERSION_VALUE);
            times = 1;
            clientCodegen.setClientPackage(CsharpDotNet2ClientOptionsProvider.CLIENT_PACKAGE_VALUE);
            times = 1;
        }};
    }
}
