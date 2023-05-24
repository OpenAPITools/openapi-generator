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
import org.openapitools.codegen.languages.PhpClientCodegen;

import java.util.Map;

public class PhpClientOptionsProvider implements OptionsProvider {
    public static final String MODEL_PACKAGE_VALUE = "package";
    public static final String API_PACKAGE_VALUE = "apiPackage";
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String SORT_MODEL_PROPERTIES_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String VARIABLE_NAMING_CONVENTION_VALUE = "snake_case";
    public static final String INVOKER_PACKAGE_VALUE = "OpenAPITools\\Client\\Php";
    public static final String PACKAGE_NAME_VALUE = "OpenAPIToolsClient-php";
    public static final String SRC_BASE_PATH_VALUE = "libPhp";
    public static final String ARTIFACT_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";
    public static final String PREPEND_FORM_OR_BODY_PARAMETERS_VALUE = "true";
    public static final String ENUM_UNKNOWN_DEFAULT_CASE_VALUE = "false";

    @Override
    public String getLanguage() {
        return "php";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.MODEL_PACKAGE, MODEL_PACKAGE_VALUE)
                .put(CodegenConstants.API_PACKAGE, API_PACKAGE_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG, SORT_MODEL_PROPERTIES_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(PhpClientCodegen.VARIABLE_NAMING_CONVENTION, VARIABLE_NAMING_CONVENTION_VALUE)
                .put(CodegenConstants.INVOKER_PACKAGE, INVOKER_PACKAGE_VALUE)
                .put(PhpClientCodegen.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(PhpClientCodegen.SRC_BASE_PATH, SRC_BASE_PATH_VALUE)
                .put(CodegenConstants.ARTIFACT_VERSION, ARTIFACT_VERSION_VALUE)
                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true")
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .put(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, PREPEND_FORM_OR_BODY_PARAMETERS_VALUE)
                .put(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "true")
                .put(CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT, "true")
                .put(CodegenConstants.ENUM_UNKNOWN_DEFAULT_CASE, ENUM_UNKNOWN_DEFAULT_CASE_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
