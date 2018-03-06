package org.openapitools.codegen.typescript.fetch;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.TypeScriptFetchClientCodegen;
import org.openapitools.codegen.options.TypeScriptFetchClientOptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class TypeScriptFetchClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private TypeScriptFetchClientCodegen clientCodegen;

    public TypeScriptFetchClientOptionsTest() {
        super(new TypeScriptFetchClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(TypeScriptFetchClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setModelPropertyNaming(TypeScriptFetchClientOptionsProvider.MODEL_PROPERTY_NAMING_VALUE);
            times = 1;
            clientCodegen.setSupportsES6(TypeScriptFetchClientOptionsProvider.SUPPORTS_ES6_VALUE);
            times = 1;
        }};
    }
}
