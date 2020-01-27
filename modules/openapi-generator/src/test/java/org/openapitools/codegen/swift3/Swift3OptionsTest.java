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

package org.openapitools.codegen.swift3;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.Swift3Codegen;
import org.openapitools.codegen.options.Swift3OptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class Swift3OptionsTest extends AbstractOptionsTest {
    private Swift3Codegen clientCodegen = mock(Swift3Codegen.class, mockSettings);

    public Swift3OptionsTest() {
        super(new Swift3OptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setSortParamsByRequiredFlag(Boolean.parseBoolean(Swift3OptionsProvider.SORT_PARAMS_VALUE));
        verify(clientCodegen).setProjectName(Swift3OptionsProvider.PROJECT_NAME_VALUE);
        verify(clientCodegen).setResponseAs(Swift3OptionsProvider.RESPONSE_AS_VALUE.split(","));
        verify(clientCodegen).setUnwrapRequired(Boolean.parseBoolean(Swift3OptionsProvider.UNWRAP_REQUIRED_VALUE));
        verify(clientCodegen).setObjcCompatible(Boolean.parseBoolean(Swift3OptionsProvider.OBJC_COMPATIBLE_VALUE));
        verify(clientCodegen).setLenientTypeCast(Boolean.parseBoolean(Swift3OptionsProvider.LENIENT_TYPE_CAST_VALUE));
        verify(clientCodegen).setPrependFormOrBodyParameters(Boolean.parseBoolean(Swift3OptionsProvider.PREPEND_FORM_OR_BODY_PARAMETERS_VALUE));
    }
}
