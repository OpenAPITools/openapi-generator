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

package org.openapitools.codegen.elixir;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.ElixirClientCodegen;
import org.openapitools.codegen.options.ElixirClientOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class ElixirClientOptionsTest extends AbstractOptionsTest {

    private ElixirClientCodegen clientCodegen = mock(ElixirClientCodegen.class, mockSettings);

    public ElixirClientOptionsTest() {
        super(new ElixirClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setModuleName(ElixirClientOptionsProvider.INVOKER_PACKAGE_VALUE);
        verify(clientCodegen).setEnumUnknownDefaultCase(Boolean.parseBoolean(ElixirClientOptionsProvider.ENUM_UNKNOWN_DEFAULT_CASE_VALUE));
    }
}