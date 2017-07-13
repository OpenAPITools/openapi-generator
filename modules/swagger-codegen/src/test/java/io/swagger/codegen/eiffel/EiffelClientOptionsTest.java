package io.swagger.codegen.eiffel;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.EiffelClientCodegen;
import io.swagger.codegen.options.EiffelClientOptionsProvider;
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
