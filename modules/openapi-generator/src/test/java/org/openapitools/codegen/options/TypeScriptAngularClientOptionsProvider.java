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

package org.openapitools.codegen.options;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.TypeScriptAngularClientCodegen;

public class TypeScriptAngularClientOptionsProvider implements OptionsProvider {
    public static final String SUPPORTS_ES6_VALUE = "false";
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String MODEL_PROPERTY_NAMING_VALUE = "camelCase";
    private static final String NMP_NAME = "npmName";
    private static final String NMP_VERSION = "1.1.2";
    private static final String NPM_REPOSITORY = "https://registry.npmjs.org";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";
    public static final String NG_VERSION = "2";
    public static final String PREPEND_FORM_OR_BODY_PARAMETERS_VALUE = "true";

    @Override
    public String getLanguage() {
        return "typescript-angular";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(CodegenConstants.MODEL_PROPERTY_NAMING, MODEL_PROPERTY_NAMING_VALUE)
                .put(CodegenConstants.SUPPORTS_ES6, SUPPORTS_ES6_VALUE)
                .put(TypeScriptAngularClientCodegen.NPM_NAME, NMP_NAME)
                .put(TypeScriptAngularClientCodegen.NPM_VERSION, NMP_VERSION)
                .put(TypeScriptAngularClientCodegen.SNAPSHOT, Boolean.FALSE.toString())
                .put(TypeScriptAngularClientCodegen.WITH_INTERFACES, Boolean.FALSE.toString())
                .put(TypeScriptAngularClientCodegen.PROVIDED_IN_ROOT, Boolean.FALSE.toString())
                .put(TypeScriptAngularClientCodegen.TAGGED_UNIONS, Boolean.FALSE.toString())
                .put(TypeScriptAngularClientCodegen.NPM_REPOSITORY, NPM_REPOSITORY)
                .put(TypeScriptAngularClientCodegen.NG_VERSION, NG_VERSION)
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .put(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, PREPEND_FORM_OR_BODY_PARAMETERS_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
