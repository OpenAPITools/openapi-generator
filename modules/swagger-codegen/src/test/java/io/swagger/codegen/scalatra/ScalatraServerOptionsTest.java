package io.swagger.codegen.scalatra;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.ScalatraServerCodegen;
import io.swagger.codegen.options.ScalatraServerOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class ScalatraServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private ScalatraServerCodegen clientCodegen;

    public ScalatraServerOptionsTest() {
        super(new ScalatraServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(ScalatraServerOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(ScalatraServerOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(ScalatraServerOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setSourceFolder(ScalatraServerOptionsProvider.SOURCE_FOLDER_VALUE);
            times = 1;
        }};
    }
}
