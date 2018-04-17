package org.openapitools.codegen.elixir;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.ElixirClientCodegen;
import org.openapitools.codegen.options.ElixirClientOptionsProvider;
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
            clientCodegen.setModuleName(ElixirClientOptionsProvider.INVOKER_PACKAGE_VALUE);
            times = 1;

        }};
    }
}
