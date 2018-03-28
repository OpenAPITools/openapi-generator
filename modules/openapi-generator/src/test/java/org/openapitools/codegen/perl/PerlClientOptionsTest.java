package org.openapitools.codegen.perl;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.PerlClientCodegen;
import org.openapitools.codegen.options.PerlClientOptionsProvider;

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
            clientCodegen.setPrependFormOrBodyParameters(Boolean.valueOf(PerlClientOptionsProvider.PREPEND_FORM_OR_BODY_PARAMETERS_VALUE));
            times = 1;
        }};
    }
}
