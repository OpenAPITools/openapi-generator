package org.openapitools.codegen.typescript.typescriptangularjs;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.TypeScriptAngularJsClientCodegen;
import org.openapitools.codegen.options.TypeScriptAngularJsClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class TypeScriptAngularJsClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private TypeScriptAngularJsClientCodegen clientCodegen;

    public TypeScriptAngularJsClientOptionsTest() {
        super(new TypeScriptAngularJsClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(TypeScriptAngularJsClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setModelPropertyNaming(TypeScriptAngularJsClientOptionsProvider.MODEL_PROPERTY_NAMING_VALUE);
            times = 1;
            clientCodegen.setSupportsES6(Boolean.valueOf(TypeScriptAngularJsClientOptionsProvider.SUPPORTS_ES6_VALUE));
            times = 1;
        }};
    }
}
