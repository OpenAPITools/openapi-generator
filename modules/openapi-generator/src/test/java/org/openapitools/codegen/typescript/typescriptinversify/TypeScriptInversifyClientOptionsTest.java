package org.openapitools.codegen.typescript.typescriptinversify;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.TypeScriptInversifyClientCodegen;
import org.openapitools.codegen.options.TypeScriptInversifyClientOptionsProvider;
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
