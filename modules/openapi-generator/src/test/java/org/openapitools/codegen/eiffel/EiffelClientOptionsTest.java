package org.openapitools.codegen.eiffel;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.EiffelClientCodegen;
import org.openapitools.codegen.options.EiffelClientOptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class EiffelClientOptionsTest  extends AbstractOptionsTest {

    @Tested
    private EiffelClientCodegen clientCodegen;

    public EiffelClientOptionsTest() {
        super(new EiffelClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setPackageName(EiffelClientOptionsProvider.PACKAGE_NAME_VALUE);
            times = 1;
        }};
    }
}
