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

package org.openapitools.codegen.typescript.fetch;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.TypeScriptFetchClientCodegen;
import org.openapitools.codegen.options.TypeScriptFetchClientOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class TypeScriptFetchClientOptionsTest extends AbstractOptionsTest {
    private TypeScriptFetchClientCodegen clientCodegen = mock(TypeScriptFetchClientCodegen.class, mockSettings);

    public TypeScriptFetchClientOptionsTest() {
        super(new TypeScriptFetchClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setSortParamsByRequiredFlag(Boolean.valueOf(TypeScriptFetchClientOptionsProvider.SORT_PARAMS_VALUE));
        verify(clientCodegen).setModelPropertyNaming(TypeScriptFetchClientOptionsProvider.MODEL_PROPERTY_NAMING_VALUE);
        verify(clientCodegen).setParamNaming(TypeScriptFetchClientOptionsProvider.PARAM_NAMING_VALUE);
        verify(clientCodegen).setSupportsES6(TypeScriptFetchClientOptionsProvider.SUPPORTS_ES6_VALUE);
        verify(clientCodegen).setPrependFormOrBodyParameters(Boolean.valueOf(TypeScriptFetchClientOptionsProvider.PREPEND_FORM_OR_BODY_PARAMETERS_VALUE));
        verify(clientCodegen).setTypescriptThreePlus(Boolean.valueOf(TypeScriptFetchClientOptionsProvider.TYPESCRIPT_THREE_PLUS));
        verify(clientCodegen).setWithoutRuntimeChecks(Boolean.valueOf(TypeScriptFetchClientOptionsProvider.WITHOUT_RUNTIME_CHECKS));
        verify(clientCodegen).setSagasAndRecords(Boolean.valueOf(TypeScriptFetchClientOptionsProvider.SAGAS_AND_RECORDS));
    }
}
