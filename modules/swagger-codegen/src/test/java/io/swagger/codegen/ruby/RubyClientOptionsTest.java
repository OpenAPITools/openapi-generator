package io.swagger.codegen.ruby;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.RubyClientCodegen;
import io.swagger.codegen.options.RubyClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class RubyClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private RubyClientCodegen clientCodegen;

    public RubyClientOptionsTest() {
        super(new RubyClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setGemName(RubyClientOptionsProvider.GEM_NAME_VALUE);
            times = 1;
            clientCodegen.setModuleName(RubyClientOptionsProvider.MODULE_NAME_VALUE);
            times = 1;
            clientCodegen.setGemVersion(RubyClientOptionsProvider.GEM_VERSION_VALUE);
            times = 1;
        }};
    }
}
