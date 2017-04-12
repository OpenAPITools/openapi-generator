package io.swagger.codegen.elixir;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.ElixirClientCodegen;
import io.swagger.codegen.options.ElixirClientOptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class ElixirClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private ElixirClientCodegen clientCodegen;

    public ElixirClientOptionsTest() {
        super(new ElixirClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            // TODO
        }};
    }
}
