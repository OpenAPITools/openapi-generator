package io.swagger.codegen.typescript.typescriptangular;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.TypeScriptAngularJsClientCodegen;
import io.swagger.codegen.options.TypeScriptAngularJsClientOptionsProvider;

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
