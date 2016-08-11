package io.swagger.codegen.asyncscala;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.AsyncScalaClientCodegen;
import io.swagger.codegen.options.AsyncScalaClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class AsyncScalaClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private AsyncScalaClientCodegen clientCodegen;

    public AsyncScalaClientOptionsTest() {
        super(new AsyncScalaClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(AsyncScalaClientOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(AsyncScalaClientOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(AsyncScalaClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
