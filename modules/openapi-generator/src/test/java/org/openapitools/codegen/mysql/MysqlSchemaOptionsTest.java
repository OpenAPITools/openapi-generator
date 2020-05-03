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

package org.openapitools.codegen.mysql;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.MysqlSchemaCodegen;
import org.openapitools.codegen.options.MysqlSchemaOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class MysqlSchemaOptionsTest extends AbstractOptionsTest {
    private MysqlSchemaCodegen clientCodegen = mock(MysqlSchemaCodegen.class, mockSettings);

    public MysqlSchemaOptionsTest() {
        super(new MysqlSchemaOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setDefaultDatabaseName(MysqlSchemaOptionsProvider.DEFAULT_DATABASE_NAME_VALUE);
        verify(clientCodegen).setJsonDataTypeEnabled(Boolean.valueOf(MysqlSchemaOptionsProvider.JSON_DATA_TYPE_ENABLED_VALUE));
        verify(clientCodegen).setIdentifierNamingConvention(MysqlSchemaOptionsProvider.IDENTIFIER_NAMING_CONVENTION_VALUE);
        verify(clientCodegen).setNamedParametersEnabled(Boolean.valueOf(MysqlSchemaOptionsProvider.NAMED_PARAMETERS_ENABLED_VALUE));
    }
}
