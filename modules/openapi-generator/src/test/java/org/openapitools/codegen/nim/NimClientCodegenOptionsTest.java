package org.openapitools.codegen.nim;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.NimClientCodegen;
import org.openapitools.codegen.options.NimClientCodegenOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class NimClientCodegenOptionsTest extends AbstractOptionsTest {

    @Tested
    private NimClientCodegen codegen;

    public NimClientCodegenOptionsTest() {
        super(new NimClientCodegenOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return codegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        // TODO: Complete options
        new Expectations(codegen) {{

        }};
    }
}

