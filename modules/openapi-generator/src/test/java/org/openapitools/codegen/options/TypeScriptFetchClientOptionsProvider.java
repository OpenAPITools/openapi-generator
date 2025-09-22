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
import org.openapitools.codegen.languages.TypeScriptFetchClientCodegen;

import java.util.Map;

import static org.openapitools.codegen.languages.TypeScriptFetchClientCodegen.PASCAL_CASE;

public class TypeScriptFetchClientOptionsProvider implements TypeScriptSharedClientOptionsProvider {
    public static final String IMPORT_FILE_EXTENSION_VALUE = "";
    public static final String WITHOUT_RUNTIME_CHECKS = "true";
    private static final String NPM_REPOSITORY = "https://registry.npmjs.org";
    public static final String SAGAS_AND_RECORDS = "false";
    public static final String STRING_ENUMS = "false";
    public static final String FILE_NAMING_VALUE = PASCAL_CASE;


    @Override
    public String getLanguage() {
        return "typescript-fetch";
    }

    @Override
    public Map<String, String> createOptions() {
        return ImmutableMap.<String, String>builder()
                .putAll(TypeScriptSharedClientOptionsProvider.super.createOptions())
                .put(TypeScriptFetchClientCodegen.NPM_REPOSITORY, NPM_REPOSITORY)
                .put(TypeScriptFetchClientCodegen.WITH_INTERFACES, Boolean.FALSE.toString())
                .put(TypeScriptFetchClientCodegen.USE_SINGLE_REQUEST_PARAMETER, Boolean.FALSE.toString())
                .put(TypeScriptFetchClientCodegen.PREFIX_PARAMETER_INTERFACES, Boolean.FALSE.toString())
                .put(TypeScriptFetchClientCodegen.WITHOUT_RUNTIME_CHECKS, WITHOUT_RUNTIME_CHECKS)
                .put(TypeScriptFetchClientCodegen.SAGAS_AND_RECORDS, SAGAS_AND_RECORDS)
                .put(TypeScriptFetchClientCodegen.IMPORT_FILE_EXTENSION_SWITCH, IMPORT_FILE_EXTENSION_VALUE)
                .put(TypeScriptFetchClientCodegen.FILE_NAMING, FILE_NAMING_VALUE)
                .put(TypeScriptFetchClientCodegen.STRING_ENUMS, STRING_ENUMS)
                .put(TypeScriptFetchClientCodegen.USE_SQUARE_BRACKETS_IN_ARRAY_NAMES, Boolean.FALSE.toString())
                .put(TypeScriptFetchClientCodegen.VALIDATION_ATTRIBUTES, Boolean.FALSE.toString())
                .build();
    }
}
