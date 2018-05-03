package org.openapitools.codegen.scalahttpclient;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.ScalaHttpClientCodegen;
import org.openapitools.codegen.options.ScalaHttpClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class ScalaHttpClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private ScalaHttpClientCodegen clientCodegen;

    public ScalaHttpClientOptionsTest() {
        super(new ScalaHttpClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(ScalaHttpClientOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(ScalaHttpClientOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(ScalaHttpClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setModelPropertyNaming(ScalaHttpClientOptionsProvider.MODEL_PROPERTY_NAMING);
            times = 1;
            clientCodegen.setSourceFolder(ScalaHttpClientOptionsProvider.SOURCE_FOLDER_VALUE);
            times = 1;
            clientCodegen.setPrependFormOrBodyParameters(Boolean.valueOf(ScalaHttpClientOptionsProvider.PREPEND_FORM_OR_BODY_PARAMETERS_VALUE));
            times = 1;
        }};
    }
}
