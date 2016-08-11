package io.swagger.codegen.perl;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.PerlClientCodegen;
import io.swagger.codegen.options.PerlClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class PerlClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private PerlClientCodegen clientCodegen;

    public PerlClientOptionsTest() {
        super(new PerlClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModuleName(PerlClientOptionsProvider.MODULE_NAME_VALUE);
            times = 1;
            clientCodegen.setModuleVersion(PerlClientOptionsProvider.MODULE_VERSION_VALUE);
            times = 1;
        }};
    }
}
