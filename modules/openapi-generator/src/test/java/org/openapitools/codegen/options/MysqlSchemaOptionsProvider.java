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
import org.openapitools.codegen.languages.MysqlSchemaCodegen;

import java.util.Map;

public class MysqlSchemaOptionsProvider implements OptionsProvider {
    public static final String DEFAULT_DATABASE_NAME_VALUE = "database_name";
    public static final String JSON_DATA_TYPE_ENABLED_VALUE = "false";
    public static final String IDENTIFIER_NAMING_CONVENTION_VALUE = "snake_case";
    public static final String NAMED_PARAMETERS_ENABLED_VALUE = "true";

    @Override
    public String getLanguage() {
        return "mysql-schema";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(MysqlSchemaCodegen.DEFAULT_DATABASE_NAME, DEFAULT_DATABASE_NAME_VALUE)
            .put(MysqlSchemaCodegen.JSON_DATA_TYPE_ENABLED, JSON_DATA_TYPE_ENABLED_VALUE)
            .put(MysqlSchemaCodegen.IDENTIFIER_NAMING_CONVENTION, IDENTIFIER_NAMING_CONVENTION_VALUE)
            .put(MysqlSchemaCodegen.NAMED_PARAMETERS_ENABLED, NAMED_PARAMETERS_ENABLED_VALUE)
            .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
