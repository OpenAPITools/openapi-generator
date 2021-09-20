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

package org.openapitools.codegen.typescript.typescriptangularjs;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.TypeScriptAngularJsClientCodegen;
import org.openapitools.codegen.options.TypeScriptAngularJsClientOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class TypeScriptAngularJsClientOptionsTest extends AbstractOptionsTest {
    private TypeScriptAngularJsClientCodegen clientCodegen = mock(TypeScriptAngularJsClientCodegen.class, mockSettings);

    public TypeScriptAngularJsClientOptionsTest() {
        super(new TypeScriptAngularJsClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setSortParamsByRequiredFlag(Boolean.valueOf(TypeScriptAngularJsClientOptionsProvider.SORT_PARAMS_VALUE));
        verify(clientCodegen).setModelPropertyNaming(TypeScriptAngularJsClientOptionsProvider.MODEL_PROPERTY_NAMING_VALUE);
        verify(clientCodegen).setParamNaming(TypeScriptAngularJsClientOptionsProvider.PARAM_NAMING_VALUE);
        verify(clientCodegen).setSupportsES6(Boolean.valueOf(TypeScriptAngularJsClientOptionsProvider.SUPPORTS_ES6_VALUE));
        verify(clientCodegen).setPrependFormOrBodyParameters(Boolean.valueOf(TypeScriptAngularJsClientOptionsProvider.PREPEND_FORM_OR_BODY_PARAMETERS_VALUE));
    }
}
