package io.swagger.codegen.typescriptnode;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.TypeScriptNodeClientCodegen;
import io.swagger.codegen.options.TypeScriptNodeClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class TypeScriptNodeClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private TypeScriptNodeClientCodegen clientCodegen;

    public TypeScriptNodeClientOptionsTest() {
        super(new TypeScriptNodeClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(TypeScriptNodeClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
