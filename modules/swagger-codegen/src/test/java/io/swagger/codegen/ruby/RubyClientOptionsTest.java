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

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setGemName(RubyClientOptionsProvider.GEM_NAME_VALUE);
            times = 1;
            clientCodegen.setModuleName(RubyClientOptionsProvider.MODULE_NAME_VALUE);
            times = 1;
            clientCodegen.setGemVersion(RubyClientOptionsProvider.GEM_VERSION_VALUE);
            times = 1;
            clientCodegen.setGemLicense(RubyClientOptionsProvider.GEM_LICENSE_VALUE);
            times = 1;
            clientCodegen.setGemRequiredRubyVersion(RubyClientOptionsProvider.GEM_REQUIRED_RUBY_VERSION_VALUE);
            times = 1;
            clientCodegen.setGemHomepage(RubyClientOptionsProvider.GEM_HOMEPAGE_VALUE);
            times = 1;
            clientCodegen.setGemDescription(RubyClientOptionsProvider.GEM_DESCRIPTION_VALUE);
            times = 1;
            clientCodegen.setGemSummary(RubyClientOptionsProvider.GEM_SUMMARY_VALUE);
            times = 1;
            clientCodegen.setGemAuthor(RubyClientOptionsProvider.GEM_AUTHOR_VALUE);
            times = 1;
            clientCodegen.setGemAuthorEmail(RubyClientOptionsProvider.GEM_AUTHOR_EMAIL_VALUE);
            times = 1;

        }};
    }
}
