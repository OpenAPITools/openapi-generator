package io.swagger.codegen.bash;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.BashClientCodegen;
import io.swagger.codegen.options.BashClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class BashClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private BashClientCodegen clientCodegen;

    public BashClientOptionsTest() {
        super(new BashClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setCurlOptions(
                BashClientOptionsProvider.CURL_OPTIONS);
            times = 1;
            clientCodegen.setProcessMarkdown(
                Boolean.parseBoolean(
                    BashClientOptionsProvider.PROCESS_MARKDOWN));
            times = 1;
            clientCodegen.setScriptName(
                BashClientOptionsProvider.SCRIPT_NAME);
            times = 1;
            clientCodegen.setGenerateBashCompletion(
                Boolean.parseBoolean(
                    BashClientOptionsProvider.GENERATE_BASH_COMPLETION));
            times = 1;
            clientCodegen.setGenerateZshCompletion(
                Boolean.parseBoolean(
                    BashClientOptionsProvider.GENERATE_ZSH_COMPLETION));
            times = 1;           
            clientCodegen.setHostEnvironmentVariable(
                BashClientOptionsProvider.HOST_ENVIRONMENT_VARIABLE_NAME);
            times = 1;            
            clientCodegen.setApiKeyAuthEnvironmentVariable(
                BashClientOptionsProvider.APIKEY_AUTH_ENVIRONMENT_VARIABLE_NAME);
            times = 1;
            clientCodegen.setAllowUnicodeIdentifiers(Boolean.valueOf(BashClientOptionsProvider.ALLOW_UNICODE_IDENTIFIERS_VALUE));
            times = 1;

        }};
    }
}

