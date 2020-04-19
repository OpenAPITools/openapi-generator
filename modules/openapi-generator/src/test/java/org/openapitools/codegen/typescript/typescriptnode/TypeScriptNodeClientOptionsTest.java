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

package org.openapitools.codegen.typescript.typescriptnode;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.TypeScriptNodeClientCodegen;
import org.openapitools.codegen.options.TypeScriptNodeClientOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class TypeScriptNodeClientOptionsTest extends AbstractOptionsTest {
    private TypeScriptNodeClientCodegen clientCodegen = mock(TypeScriptNodeClientCodegen.class, mockSettings);

    public TypeScriptNodeClientOptionsTest() {
        super(new TypeScriptNodeClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setSortParamsByRequiredFlag(Boolean.valueOf(TypeScriptNodeClientOptionsProvider.SORT_PARAMS_VALUE));
        verify(clientCodegen).setModelPropertyNaming(TypeScriptNodeClientOptionsProvider.MODEL_PROPERTY_NAMING_VALUE);
        verify(clientCodegen).setSupportsES6(Boolean.valueOf(TypeScriptNodeClientOptionsProvider.SUPPORTS_ES6_VALUE));
        verify(clientCodegen).setPrependFormOrBodyParameters(Boolean.valueOf(TypeScriptNodeClientOptionsProvider.PREPEND_FORM_OR_BODY_PARAMETERS_VALUE));
    }
}
