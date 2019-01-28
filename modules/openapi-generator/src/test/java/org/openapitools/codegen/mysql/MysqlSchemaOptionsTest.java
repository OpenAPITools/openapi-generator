/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.mysql;

import mockit.Expectations;
import mockit.Tested;
import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.MysqlSchemaCodegen;
import org.openapitools.codegen.options.MysqlSchemaOptionsProvider;

public class MysqlSchemaOptionsTest extends AbstractOptionsTest {

    @Tested
    private MysqlSchemaCodegen clientCodegen;

    public MysqlSchemaOptionsTest() {
        super(new MysqlSchemaOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setDefaultDatabaseName(MysqlSchemaOptionsProvider.DEFAULT_DATABASE_NAME_VALUE);
            times = 1;
            clientCodegen.setJsonDataTypeEnabled(Boolean.valueOf(MysqlSchemaOptionsProvider.JSON_DATA_TYPE_ENABLED_VALUE));
            times = 1;
        }};
    }
}
