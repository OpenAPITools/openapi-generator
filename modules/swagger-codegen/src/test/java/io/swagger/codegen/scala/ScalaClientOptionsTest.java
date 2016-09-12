package io.swagger.codegen.scala;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.ScalaClientCodegen;
import io.swagger.codegen.options.ScalaClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class ScalaClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private ScalaClientCodegen clientCodegen;

    public ScalaClientOptionsTest() {
        super(new ScalaClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(ScalaClientOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(ScalaClientOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(ScalaClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setModelPropertyNaming(ScalaClientOptionsProvider.MODEL_PROPERTY_NAMING);
            times = 1;
            clientCodegen.setSourceFolder(ScalaClientOptionsProvider.SOURCE_FOLDER_VALUE);
            times = 1;
        }};
    }
}
