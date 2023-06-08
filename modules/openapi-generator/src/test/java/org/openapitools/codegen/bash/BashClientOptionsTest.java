/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.bash;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.BashClientCodegen;
import org.openapitools.codegen.options.BashClientOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class BashClientOptionsTest extends AbstractOptionsTest {
    private BashClientCodegen clientCodegen = mock(BashClientCodegen.class, mockSettings);

    public BashClientOptionsTest() {
        super(new BashClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setCurlOptions(
                BashClientOptionsProvider.CURL_OPTIONS);
        verify(clientCodegen).setProcessMarkdown(
                Boolean.parseBoolean(
                        BashClientOptionsProvider.PROCESS_MARKDOWN));
        verify(clientCodegen).setScriptName(
                BashClientOptionsProvider.SCRIPT_NAME);
        verify(clientCodegen).setGenerateBashCompletion(
                Boolean.parseBoolean(
                        BashClientOptionsProvider.GENERATE_BASH_COMPLETION));
        verify(clientCodegen).setGenerateZshCompletion(
                Boolean.parseBoolean(
                        BashClientOptionsProvider.GENERATE_ZSH_COMPLETION));
        verify(clientCodegen).setHostEnvironmentVariable(
                BashClientOptionsProvider.HOST_ENVIRONMENT_VARIABLE_NAME);
        verify(clientCodegen).setApiKeyAuthEnvironmentVariable(
                BashClientOptionsProvider.APIKEY_AUTH_ENVIRONMENT_VARIABLE_NAME);
        verify(clientCodegen).setAllowUnicodeIdentifiers(Boolean.valueOf(BashClientOptionsProvider.ALLOW_UNICODE_IDENTIFIERS_VALUE));
        verify(clientCodegen).setEnumUnknownDefaultCase(Boolean.parseBoolean(BashClientOptionsProvider.ENUM_UNKNOWN_DEFAULT_CASE_VALUE));
    }
}

