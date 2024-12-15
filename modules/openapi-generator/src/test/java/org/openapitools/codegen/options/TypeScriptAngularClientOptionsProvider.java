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
import org.openapitools.codegen.languages.TypeScriptAngularClientCodegen;

import java.util.Map;

public class TypeScriptAngularClientOptionsProvider implements TypeScriptSharedClientOptionsProvider {
    public static final String STRING_ENUMS_VALUE = "false";
    private static final String NPM_REPOSITORY = "https://registry.npmjs.org";
    public static final String NG_VERSION = "19";
    public static final String FILE_NAMING_VALUE = "camelCase";
    public static final String API_MODULE_PREFIX = "";
    public static final String CONFIGURATION_PREFIX = "";
    public static final String QUERY_PARAM_OBJECT_FORMAT_VALUE = "dot";
    public static final String PROVIDED_IN_LEVEL = "root";
    public static final String SERVICE_SUFFIX = "Service";
    public static final String SERVICE_FILE_SUFFIX = ".service";
    public static final String MODEL_SUFFIX = "";
    public static final String MODEL_FILE_SUFFIX = "";
    public static final String TS_VERSION = "";
    public static final String RXJS_VERSION = "";
    public static final String NGPACKAGR_VERSION = "";
    public static final String ZONEJS_VERSION = "";

    @Override
    public String getLanguage() {
        return "typescript-angular";
    }

    @Override
    public Map<String, String> createOptions() {
        return ImmutableMap.<String, String>builder()
                .putAll(TypeScriptSharedClientOptionsProvider.super.createOptions())
                .put(TypeScriptAngularClientCodegen.STRING_ENUMS, STRING_ENUMS_VALUE)
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
                .put(TypeScriptAngularClientCodegen.FILE_NAMING, FILE_NAMING_VALUE)
                .put(TypeScriptAngularClientCodegen.QUERY_PARAM_OBJECT_FORMAT, QUERY_PARAM_OBJECT_FORMAT_VALUE)
                .put(TypeScriptAngularClientCodegen.USE_SQUARE_BRACKETS_IN_ARRAY_NAMES, Boolean.FALSE.toString())
                .put(TypeScriptAngularClientCodegen.TS_VERSION, TS_VERSION)
                .put(TypeScriptAngularClientCodegen.RXJS_VERSION, RXJS_VERSION)
                .put(TypeScriptAngularClientCodegen.NGPACKAGR_VERSION, NGPACKAGR_VERSION)
                .put(TypeScriptAngularClientCodegen.ZONEJS_VERSION, ZONEJS_VERSION)
                .build();
    }
}
