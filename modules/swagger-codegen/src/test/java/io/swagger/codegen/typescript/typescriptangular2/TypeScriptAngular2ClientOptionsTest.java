package io.swagger.codegen.typescript.typescriptangular2;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.TypeScriptAngular2ClientCodegen;
import io.swagger.codegen.options.TypeScriptAngular2ClientOptionsProvider;
import io.swagger.codegen.options.TypeScriptAngularClientOptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class TypeScriptAngular2ClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private TypeScriptAngular2ClientCodegen clientCodegen;

    public TypeScriptAngular2ClientOptionsTest() {
        super(new TypeScriptAngular2ClientOptionsProvider());
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
            clientCodegen.setNgVersion(TypeScriptAngular2ClientOptionsProvider.NG_VERSION);
            times = 1;
            clientCodegen.setSupportsES6(Boolean.valueOf(TypeScriptAngularClientOptionsProvider.SUPPORTS_ES6_VALUE));
            times = 1;
        }};
    }
}
