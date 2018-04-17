package io.swagger.codegen.typescript.typescriptinversify;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.TypeScriptInversifyClientCodegen;
import io.swagger.codegen.options.TypeScriptInversifyClientOptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class TypeScriptInversifyClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private TypeScriptInversifyClientCodegen clientCodegen;

    public TypeScriptInversifyClientOptionsTest() {
        super(new TypeScriptInversifyClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(TypeScriptInversifyClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setModelPropertyNaming(TypeScriptInversifyClientOptionsProvider.MODEL_PROPERTY_NAMING_VALUE);
            times = 1;
            clientCodegen.setSupportsES6(Boolean.valueOf(TypeScriptInversifyClientOptionsProvider.SUPPORTS_ES6_VALUE));
            times = 1;
        }};
    }
}
