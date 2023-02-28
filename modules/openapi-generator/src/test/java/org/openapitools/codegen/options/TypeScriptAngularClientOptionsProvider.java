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
import org.openapitools.codegen.languages.AbstractTypeScriptClientCodegen;
import org.openapitools.codegen.languages.TypeScriptAngularClientCodegen;

import java.util.Map;

public class TypeScriptAngularClientOptionsProvider implements OptionsProvider {
    public static final String SUPPORTS_ES6_VALUE = "false";
    public static final String NULL_SAFE_ADDITIONAL_PROPS_VALUE = "false";
    public static final String ENUM_NAME_SUFFIX = "Enum";
    public static final String STRING_ENUMS_VALUE = "false";
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String SORT_MODEL_PROPERTIES_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String ENUM_PROPERTY_NAMING_VALUE = "PascalCase";
    public static final String MODEL_PROPERTY_NAMING_VALUE = "camelCase";
    public static final String PARAM_NAMING_VALUE = "camelCase";
    private static final String NPM_NAME = "npmName";
    private static final String NPM_VERSION = "1.1.2";
    private static final String NPM_REPOSITORY = "https://registry.npmjs.org";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";
    public static final String NG_VERSION = "12";
    public static final String PREPEND_FORM_OR_BODY_PARAMETERS_VALUE = "true";
    public static final String FILE_NAMING_VALUE = "camelCase";
    public static final String API_MODULE_PREFIX = "";
    public static final String CONFIGURATION_PREFIX = "";
    public static final String QUERY_PARAM_OBJECT_FORMAT_VALUE = "dot";
    public static final String PROVIDED_IN_LEVEL = "root";
    public static String SERVICE_SUFFIX = "Service";
    public static String SERVICE_FILE_SUFFIX = ".service";
    public static String MODEL_SUFFIX = "";
    public static String MODEL_FILE_SUFFIX = "";
    public static final String ENUM_UNKNOWN_DEFAULT_CASE_VALUE = "false";

    @Override
    public String getLanguage() {
        return "typescript-angular";
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
                .put(CodegenConstants.SUPPORTS_ES6, SUPPORTS_ES6_VALUE)
                .put(AbstractTypeScriptClientCodegen.NULL_SAFE_ADDITIONAL_PROPS, NULL_SAFE_ADDITIONAL_PROPS_VALUE)
                .put(CodegenConstants.ENUM_NAME_SUFFIX, ENUM_NAME_SUFFIX)
                .put(TypeScriptAngularClientCodegen.STRING_ENUMS, STRING_ENUMS_VALUE)
                .put(TypeScriptAngularClientCodegen.NPM_NAME, NPM_NAME)
                .put(TypeScriptAngularClientCodegen.NPM_VERSION, NPM_VERSION)
                .put(TypeScriptAngularClientCodegen.SNAPSHOT, Boolean.FALSE.toString())
                .put(TypeScriptAngularClientCodegen.WITH_INTERFACES, Boolean.FALSE.toString())
                .put(TypeScriptAngularClientCodegen.USE_SINGLE_REQUEST_PARAMETER, Boolean.FALSE.toString())
                .put(TypeScriptAngularClientCodegen.PROVIDED_IN, PROVIDED_IN_LEVEL)
                .put(TypeScriptAngularClientCodegen.TAGGED_UNIONS, Boolean.FALSE.toString())
                .put(TypeScriptAngularClientCodegen.NPM_REPOSITORY, NPM_REPOSITORY)
                .put(TypeScriptAngularClientCodegen.NG_VERSION, NG_VERSION)
                .put(TypeScriptAngularClientCodegen.API_MODULE_PREFIX, API_MODULE_PREFIX)
                .put(TypeScriptAngularClientCodegen.CONFIGURATION_PREFIX, CONFIGURATION_PREFIX)
                .put(TypeScriptAngularClientCodegen.SERVICE_SUFFIX, SERVICE_SUFFIX)
                .put(TypeScriptAngularClientCodegen.SERVICE_FILE_SUFFIX, SERVICE_FILE_SUFFIX)
                .put(TypeScriptAngularClientCodegen.MODEL_SUFFIX, MODEL_SUFFIX)
                .put(TypeScriptAngularClientCodegen.MODEL_FILE_SUFFIX, MODEL_FILE_SUFFIX)
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .put(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, PREPEND_FORM_OR_BODY_PARAMETERS_VALUE)
                .put(TypeScriptAngularClientCodegen.FILE_NAMING, FILE_NAMING_VALUE)
                .put(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "true")
                .put(CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT, "true")
                .put(TypeScriptAngularClientCodegen.QUERY_PARAM_OBJECT_FORMAT, QUERY_PARAM_OBJECT_FORMAT_VALUE)
                .put(CodegenConstants.ENUM_UNKNOWN_DEFAULT_CASE, ENUM_UNKNOWN_DEFAULT_CASE_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
