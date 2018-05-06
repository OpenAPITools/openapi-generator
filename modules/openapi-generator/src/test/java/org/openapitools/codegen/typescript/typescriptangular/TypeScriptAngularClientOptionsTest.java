package org.openapitools.codegen.typescript.typescriptangular;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.TypeScriptAngularClientCodegen;
import org.openapitools.codegen.options.TypeScriptAngularClientOptionsProvider;
import org.openapitools.codegen.options.TypeScriptAngularClientOptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class TypeScriptAngularClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private TypeScriptAngularClientCodegen clientCodegen;

    public TypeScriptAngularClientOptionsTest() {
        super(new TypeScriptAngularClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(TypeScriptAngularClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setModelPropertyNaming(TypeScriptAngularClientOptionsProvider.MODEL_PROPERTY_NAMING_VALUE);
            times = 1;
            clientCodegen.setSupportsES6(Boolean.valueOf(TypeScriptAngularClientOptionsProvider.SUPPORTS_ES6_VALUE));
            times = 1;
            clientCodegen.setPrependFormOrBodyParameters(Boolean.valueOf(TypeScriptAngularClientOptionsProvider.PREPEND_FORM_OR_BODY_PARAMETERS_VALUE));
            times = 1;
        }};
    }
}
