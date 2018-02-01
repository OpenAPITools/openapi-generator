package io.swagger.codegen.typescript.typescriptangular;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.TypeScriptAngularClientCodegen;
import io.swagger.codegen.options.TypeScriptAngularClientOptionsProvider;
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
        }};
    }
}
