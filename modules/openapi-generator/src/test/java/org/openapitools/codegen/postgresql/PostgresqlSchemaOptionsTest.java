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

package org.openapitools.codegen.postgresql;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.PostgresqlSchemaCodegen;
import org.openapitools.codegen.options.PostgresqlSchemaOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class PostgresqlSchemaOptionsTest extends AbstractOptionsTest {
    private PostgresqlSchemaCodegen clientCodegen = mock(PostgresqlSchemaCodegen.class, mockSettings);

    public PostgresqlSchemaOptionsTest() {
        super(new PostgresqlSchemaOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setDefaultDatabaseName(PostgresqlSchemaOptionsProvider.DEFAULT_DATABASE_NAME_VALUE);
        verify(clientCodegen).setJsonDataType(PostgresqlSchemaOptionsProvider.JSON_DATA_TYPE_VALUE);
        verify(clientCodegen).setIdentifierNamingConvention(PostgresqlSchemaOptionsProvider.IDENTIFIER_NAMING_CONVENTION_VALUE);
        verify(clientCodegen).setNamedParametersEnabled(Boolean.valueOf(PostgresqlSchemaOptionsProvider.NAMED_PARAMETERS_ENABLED_VALUE));
        verify(clientCodegen).setIdAutoIncEnabled(Boolean.valueOf(PostgresqlSchemaOptionsProvider.ID_AUTOINC_ENABLED_VALUE));
    }
}
