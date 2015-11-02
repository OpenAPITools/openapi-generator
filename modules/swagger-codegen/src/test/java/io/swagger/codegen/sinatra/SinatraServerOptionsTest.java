package io.swagger.codegen.sinatra;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.SinatraServerCodegen;
import io.swagger.codegen.options.SinatraServerOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class SinatraServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private SinatraServerCodegen clientCodegen;

    public SinatraServerOptionsTest() {
        super(new SinatraServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
        }};
    }
}
