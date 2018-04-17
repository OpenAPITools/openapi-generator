package org.openapitools.codegen.haskellservant;


import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.HaskellServantCodegen;
import org.openapitools.codegen.options.HaskellServantOptionsProvider;

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
