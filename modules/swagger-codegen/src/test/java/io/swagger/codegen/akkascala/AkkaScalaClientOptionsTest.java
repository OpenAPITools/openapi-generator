package io.swagger.codegen.akkascala;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.AkkaScalaClientCodegen;
import io.swagger.codegen.options.AkkaScalaClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class AkkaScalaClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private AkkaScalaClientCodegen clientCodegen;

    public AkkaScalaClientOptionsTest() {
        super(new AkkaScalaClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(AkkaScalaClientOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(AkkaScalaClientOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(AkkaScalaClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
