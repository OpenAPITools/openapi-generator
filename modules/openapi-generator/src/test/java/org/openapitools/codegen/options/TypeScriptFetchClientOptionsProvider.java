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
import org.openapitools.codegen.languages.TypeScriptFetchClientCodegen;
import org.openapitools.codegen.languages.AbstractTypeScriptClientCodegen;

import java.util.Map;

public class TypeScriptFetchClientOptionsProvider implements OptionsProvider {
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String SORT_MODEL_PROPERTIES_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final Boolean SUPPORTS_ES6_VALUE = false;
    public static final Boolean NULL_SAFE_ADDITIONAL_PROPS_VALUE = false;
    public static final String ENUM_NAME_SUFFIX = "Enum";
    public static final String MODEL_PROPERTY_NAMING_VALUE = "camelCase";
    public static final String PARAM_NAMING_VALUE = "camelCase";
    public static final String ENUM_PROPERTY_NAMING_VALUE = "PascalCase";
    private static final String NMP_NAME = "npmName";
    private static final String NMP_VERSION = "1.0.0";
    private static final String NPM_REPOSITORY = "https://registry.npmjs.org";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";
    public static final String PREPEND_FORM_OR_BODY_PARAMETERS_VALUE = "true";
    public static final String TYPESCRIPT_THREE_PLUS = "true";
    public static final String WITHOUT_RUNTIME_CHECKS = "true";
    public static final String SAGAS_AND_RECORDS = "false";
    public static final String ENUM_UNKNOWN_DEFAULT_CASE_VALUE = "false";

    @Override
    public String getLanguage() {
        return "typescript-fetch";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG, SORT_MODEL_PROPERTIES_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(CodegenConstants.ENUM_PROPERTY_NAMING, ENUM_PROPERTY_NAMING_VALUE)
                .put(CodegenConstants.MODEL_PROPERTY_NAMING, MODEL_PROPERTY_NAMING_VALUE)
                .put(CodegenConstants.PARAM_NAMING, PARAM_NAMING_VALUE)
                .put(CodegenConstants.SUPPORTS_ES6, String.valueOf(SUPPORTS_ES6_VALUE))
                .put(AbstractTypeScriptClientCodegen.NULL_SAFE_ADDITIONAL_PROPS, String.valueOf(NULL_SAFE_ADDITIONAL_PROPS_VALUE))
                .put(CodegenConstants.ENUM_NAME_SUFFIX, ENUM_NAME_SUFFIX)
                .put(TypeScriptFetchClientCodegen.NPM_NAME, NMP_NAME)
                .put(TypeScriptFetchClientCodegen.NPM_VERSION, NMP_VERSION)
                .put(TypeScriptFetchClientCodegen.NPM_REPOSITORY, NPM_REPOSITORY)
                .put(TypeScriptFetchClientCodegen.SNAPSHOT, Boolean.FALSE.toString())
                .put(TypeScriptFetchClientCodegen.WITH_INTERFACES, Boolean.FALSE.toString())
                .put(TypeScriptFetchClientCodegen.USE_SINGLE_REQUEST_PARAMETER, Boolean.FALSE.toString())
                .put(TypeScriptFetchClientCodegen.PREFIX_PARAMETER_INTERFACES, Boolean.FALSE.toString())
                .put(TypeScriptFetchClientCodegen.TYPESCRIPT_THREE_PLUS, TYPESCRIPT_THREE_PLUS)
                .put(TypeScriptFetchClientCodegen.WITHOUT_RUNTIME_CHECKS, WITHOUT_RUNTIME_CHECKS)
                .put(TypeScriptFetchClientCodegen.SAGAS_AND_RECORDS, SAGAS_AND_RECORDS)
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
