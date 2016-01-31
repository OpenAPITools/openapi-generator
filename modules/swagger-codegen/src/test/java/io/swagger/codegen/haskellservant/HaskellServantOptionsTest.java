package io.swagger.codegen.haskellservant;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.HaskellServantCodegen;
import io.swagger.codegen.options.HaskellServantOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class HaskellServantOptionsTest extends AbstractOptionsTest {

    @Tested
    private HaskellServantCodegen clientCodegen;

    public HaskellServantOptionsTest() {
        super(new HaskellServantOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(HaskellServantOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(HaskellServantOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(HaskellServantOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
