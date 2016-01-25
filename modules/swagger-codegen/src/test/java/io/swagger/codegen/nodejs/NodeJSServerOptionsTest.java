package io.swagger.codegen.nodejs;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.NodeJSServerCodegen;
import io.swagger.codegen.options.NodeJSServerOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class NodeJSServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private NodeJSServerCodegen clientCodegen;

    public NodeJSServerOptionsTest() {
        super(new NodeJSServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(NodeJSServerOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
