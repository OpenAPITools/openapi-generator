/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.bash;

import mockit.Expectations;
import mockit.Tested;
import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.BashClientCodegen;
import org.openapitools.codegen.options.BashClientOptionsProvider;

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

