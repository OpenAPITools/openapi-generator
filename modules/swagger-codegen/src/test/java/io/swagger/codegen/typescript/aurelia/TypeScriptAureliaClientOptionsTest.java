package io.swagger.codegen.typescript.aurelia;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.TypeScriptAureliaClientCodegen;
import io.swagger.codegen.options.TypeScriptAureliaClientOptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class TypeScriptAureliaClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private TypeScriptAureliaClientCodegen clientCodegen;

    public TypeScriptAureliaClientOptionsTest() {
        super(new TypeScriptAureliaClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(TypeScriptAureliaClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setModelPropertyNaming(TypeScriptAureliaClientOptionsProvider.MODEL_PROPERTY_NAMING_VALUE);
            times = 1;
            clientCodegen.setSupportsES6(TypeScriptAureliaClientOptionsProvider.SUPPORTS_ES6_VALUE);
            times = 1;
        }};
    }
}
