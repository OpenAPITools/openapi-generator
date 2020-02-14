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

package org.openapitools.codegen.swift4;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.Swift4Codegen;
import org.openapitools.codegen.options.Swift4OptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class Swift4OptionsTest extends AbstractOptionsTest {
    private Swift4Codegen clientCodegen = mock(Swift4Codegen.class, mockSettings);

    public Swift4OptionsTest() {
        super(new Swift4OptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setSortParamsByRequiredFlag(Boolean.valueOf(Swift4OptionsProvider.SORT_PARAMS_VALUE));
        verify(clientCodegen).setProjectName(Swift4OptionsProvider.PROJECT_NAME_VALUE);
        verify(clientCodegen).setResponseAs(Swift4OptionsProvider.RESPONSE_AS_VALUE.split(","));
        verify(clientCodegen).setNonPublicApi(Boolean.parseBoolean(Swift4OptionsProvider.NON_PUBLIC_API_REQUIRED_VALUE));
        verify(clientCodegen).setUnwrapRequired(Boolean.parseBoolean(Swift4OptionsProvider.UNWRAP_REQUIRED_VALUE));
        verify(clientCodegen).setObjcCompatible(Boolean.parseBoolean(Swift4OptionsProvider.OBJC_COMPATIBLE_VALUE));
        verify(clientCodegen).setLenientTypeCast(Boolean.parseBoolean(Swift4OptionsProvider.LENIENT_TYPE_CAST_VALUE));
        verify(clientCodegen).setPrependFormOrBodyParameters(Boolean.valueOf(Swift4OptionsProvider.PREPEND_FORM_OR_BODY_PARAMETERS_VALUE));
    }
}
