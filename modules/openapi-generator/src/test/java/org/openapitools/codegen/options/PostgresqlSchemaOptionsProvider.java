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
import org.openapitools.codegen.languages.PostgresqlSchemaCodegen;

import java.util.Map;

public class PostgresqlSchemaOptionsProvider implements OptionsProvider {
    public static final String DEFAULT_DATABASE_NAME_VALUE = "database_name";
    public static final String JSON_DATA_TYPE_VALUE = "json";
    public static final String IDENTIFIER_NAMING_CONVENTION_VALUE = "snake_case";
    public static final String NAMED_PARAMETERS_ENABLED_VALUE = "true";
    public static final String ID_AUTOINC_ENABLED_VALUE = "false";

    @Override
    public String getLanguage() {
        return "postgresql-schema";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(PostgresqlSchemaCodegen.DEFAULT_DATABASE_NAME, DEFAULT_DATABASE_NAME_VALUE)
                .put(PostgresqlSchemaCodegen.JSON_DATA_TYPE, JSON_DATA_TYPE_VALUE)
                .put(PostgresqlSchemaCodegen.IDENTIFIER_NAMING_CONVENTION, IDENTIFIER_NAMING_CONVENTION_VALUE)
                .put(PostgresqlSchemaCodegen.NAMED_PARAMETERS_ENABLED, NAMED_PARAMETERS_ENABLED_VALUE)
                .put(PostgresqlSchemaCodegen.ID_AUTOINC_ENABLED, ID_AUTOINC_ENABLED_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
