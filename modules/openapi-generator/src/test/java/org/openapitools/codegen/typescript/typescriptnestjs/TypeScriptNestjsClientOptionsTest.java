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

package org.openapitools.codegen.typescript.typescriptnestjs;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.TypeScriptNestjsClientCodegen;
import org.openapitools.codegen.options.TypeScriptNestjsClientOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class TypeScriptNestjsClientOptionsTest extends AbstractOptionsTest {

    private TypeScriptNestjsClientCodegen clientCodegen = mock(TypeScriptNestjsClientCodegen.class, mockSettings);

    public TypeScriptNestjsClientOptionsTest() {
        super(new TypeScriptNestjsClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setSortParamsByRequiredFlag(Boolean.valueOf(TypeScriptNestjsClientOptionsProvider.SORT_PARAMS_VALUE));
        verify(clientCodegen).setModelPropertyNaming(TypeScriptNestjsClientOptionsProvider.MODEL_PROPERTY_NAMING_VALUE);
        verify(clientCodegen).setParamNaming(TypeScriptNestjsClientOptionsProvider.PARAM_NAMING_VALUE);
        verify(clientCodegen).setSupportsES6(Boolean.valueOf(TypeScriptNestjsClientOptionsProvider.SUPPORTS_ES6_VALUE));
        verify(clientCodegen).setStringEnums(Boolean.parseBoolean(TypeScriptNestjsClientOptionsProvider.STRING_ENUMS_VALUE));
        verify(clientCodegen).setPrependFormOrBodyParameters(Boolean.valueOf(TypeScriptNestjsClientOptionsProvider.PREPEND_FORM_OR_BODY_PARAMETERS_VALUE));
        verify(clientCodegen).setEnumUnknownDefaultCase(Boolean.parseBoolean(TypeScriptNestjsClientOptionsProvider.ENUM_UNKNOWN_DEFAULT_CASE_VALUE));
    }
}
