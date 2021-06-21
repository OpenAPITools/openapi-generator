/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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
import org.openapitools.codegen.languages.TypeScriptNestjsClientCodegen;

import java.util.Map;

public class TypeScriptNestjsClientOptionsProvider implements OptionsProvider {
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
    private static final String NMP_NAME = "npmName";
    private static final String NMP_VERSION = "1.1.2";
    private static final String NPM_REPOSITORY = "https://registry.npmjs.org";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";
    public static final String NEST_VERSION = "6";
    public static final String PREPEND_FORM_OR_BODY_PARAMETERS_VALUE = "true";
    public static final String FILE_NAMING_VALUE = "camelCase";
    public static final String API_MODULE_PREFIX = "";
    public static String SERVICE_SUFFIX = "Service";
    public static String SERVICE_FILE_SUFFIX = ".service";
    public static String MODEL_SUFFIX = "";
    public static String MODEL_FILE_SUFFIX = "";

    @Override
    public String getLanguage() {
        return "typescript-nestjs";
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
                .put(TypeScriptNestjsClientCodegen.STRING_ENUMS, STRING_ENUMS_VALUE)
                .put(TypeScriptNestjsClientCodegen.NPM_NAME, NMP_NAME)
                .put(TypeScriptNestjsClientCodegen.NPM_VERSION, NMP_VERSION)
                .put(TypeScriptNestjsClientCodegen.SNAPSHOT, Boolean.FALSE.toString())
                .put(TypeScriptNestjsClientCodegen.WITH_INTERFACES, Boolean.FALSE.toString())
                //.put(TypeScriptNestjsClientCodegen.USE_SINGLE_REQUEST_PARAMETER, Boolean.FALSE.toString())
                //.put(TypeScriptNestjsClientCodegen.PROVIDED_IN_ROOT, Boolean.FALSE.toString())
                .put(TypeScriptNestjsClientCodegen.TAGGED_UNIONS, Boolean.FALSE.toString())
                .put(TypeScriptNestjsClientCodegen.NPM_REPOSITORY, NPM_REPOSITORY)
                .put(TypeScriptNestjsClientCodegen.NEST_VERSION, NEST_VERSION)
                //.put(TypeScriptNestjsClientCodegen.API_MODULE_PREFIX, API_MODULE_PREFIX)
                .put(TypeScriptNestjsClientCodegen.SERVICE_SUFFIX, SERVICE_SUFFIX)
                .put(TypeScriptNestjsClientCodegen.SERVICE_FILE_SUFFIX, SERVICE_FILE_SUFFIX)
                .put(TypeScriptNestjsClientCodegen.MODEL_SUFFIX, MODEL_SUFFIX)
                .put(TypeScriptNestjsClientCodegen.MODEL_FILE_SUFFIX, MODEL_FILE_SUFFIX)
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .put(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, PREPEND_FORM_OR_BODY_PARAMETERS_VALUE)
                .put(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "true")
                .put(CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT, "true")
                .put(TypeScriptNestjsClientCodegen.FILE_NAMING, FILE_NAMING_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
