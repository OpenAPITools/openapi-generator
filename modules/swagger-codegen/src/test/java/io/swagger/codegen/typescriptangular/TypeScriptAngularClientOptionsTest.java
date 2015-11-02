package io.swagger.codegen.typescriptangular;

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

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(TypeScriptAngularClientOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(TypeScriptAngularClientOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(TypeScriptAngularClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
