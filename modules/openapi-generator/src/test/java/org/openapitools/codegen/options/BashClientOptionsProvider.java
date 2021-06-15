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

package org.openapitools.codegen.options;

import com.google.common.collect.ImmutableMap;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.BashClientCodegen;

import java.util.Map;

public class BashClientOptionsProvider implements OptionsProvider {

    public static final String CURL_OPTIONS = "-k --tlsv1.2";
    public static final String PROCESS_MARKDOWN = "true";
    public static final String SCRIPT_NAME = "petstore-cli";
    public static final String GENERATE_BASH_COMPLETION = "true";
    public static final String GENERATE_ZSH_COMPLETION = "false";
    public static final String HOST_ENVIRONMENT_VARIABLE_NAME
                                = "PETSTORE_HOSTNAME";
    public static final String BASIC_AUTH_ENVIRONMENT_VARIABLE_NAME
                                = "PETSTORE_BASIC_AUTH";
    public static final String APIKEY_AUTH_ENVIRONMENT_VARIABLE_NAME
                                = "PETSTORE_APIKEY";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE
                                = "false";
    public static final String PREPEND_FORM_OR_BODY_PARAMETERS_VALUE = "true";

    @Override
    public String getLanguage() {
        return "bash";
    }

    @Override
    public Map<String, String> createOptions() {

        ImmutableMap.Builder<String, String> builder
            = new ImmutableMap.Builder<String, String>();

        return builder
                .put(BashClientCodegen.CURL_OPTIONS, CURL_OPTIONS)
                .put(BashClientCodegen.SCRIPT_NAME, SCRIPT_NAME)
                .put(BashClientCodegen.PROCESS_MARKDOWN, PROCESS_MARKDOWN)
                .put(BashClientCodegen.GENERATE_BASH_COMPLETION,
                        GENERATE_BASH_COMPLETION)
                .put(BashClientCodegen.GENERATE_ZSH_COMPLETION,
                        GENERATE_ZSH_COMPLETION)
                .put(BashClientCodegen.HOST_ENVIRONMENT_VARIABLE_NAME,
                        HOST_ENVIRONMENT_VARIABLE_NAME)
                .put(BashClientCodegen.BASIC_AUTH_ENVIRONMENT_VARIABLE_NAME,
                        BASIC_AUTH_ENVIRONMENT_VARIABLE_NAME)
                .put(BashClientCodegen.APIKEY_AUTH_ENVIRONMENT_VARIABLE_NAME,
                        APIKEY_AUTH_ENVIRONMENT_VARIABLE_NAME)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, "false")
                .put(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG, "false")
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, "false")
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .put(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, PREPEND_FORM_OR_BODY_PARAMETERS_VALUE)
                .put(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "true")
                .put(CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT, "true")
                .build();

    }

    @Override
    public boolean isServer() {

        return false;

    }
}
