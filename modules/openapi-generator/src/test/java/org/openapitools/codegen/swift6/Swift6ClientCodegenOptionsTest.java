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

package org.openapitools.codegen.swift6;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.Swift6ClientCodegen;
import org.openapitools.codegen.options.Swift6ClientCodegenOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class Swift6ClientCodegenOptionsTest extends AbstractOptionsTest {
    private Swift6ClientCodegen clientCodegen = mock(Swift6ClientCodegen.class, mockSettings);

    public Swift6ClientCodegenOptionsTest() {
        super(new Swift6ClientCodegenOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setSortParamsByRequiredFlag(Boolean.valueOf(Swift6ClientCodegenOptionsProvider.SORT_PARAMS_VALUE));
        verify(clientCodegen).setProjectName(Swift6ClientCodegenOptionsProvider.PROJECT_NAME_VALUE);
        verify(clientCodegen).setResponseAs(Swift6ClientCodegenOptionsProvider.RESPONSE_AS_VALUE.split(","));
        verify(clientCodegen)
                .setNonPublicApi(Boolean.parseBoolean(Swift6ClientCodegenOptionsProvider.NON_PUBLIC_API_REQUIRED_VALUE));
        verify(clientCodegen).setObjcCompatible(Boolean.parseBoolean(Swift6ClientCodegenOptionsProvider.OBJC_COMPATIBLE_VALUE));
        verify(clientCodegen).setPrependFormOrBodyParameters(
                Boolean.valueOf(Swift6ClientCodegenOptionsProvider.PREPEND_FORM_OR_BODY_PARAMETERS_VALUE));
        verify(clientCodegen)
                .setReadonlyProperties(Boolean.parseBoolean(Swift6ClientCodegenOptionsProvider.READONLY_PROPERTIES_VALUE));
        verify(clientCodegen).setGenerateModelAdditionalProperties(
                Boolean.parseBoolean(Swift6ClientCodegenOptionsProvider.GENERATE_MODEL_ADDITIONAL_PROPERTIES_VALUE));
        verify(clientCodegen).setHashableModels(Boolean.parseBoolean(Swift6ClientCodegenOptionsProvider.HASHABLE_MODELS_VALUE));
        verify(clientCodegen).setIdentifiableModels(Boolean.parseBoolean(Swift6ClientCodegenOptionsProvider.IDENTIFIABLE_MODELS_VALUE));
        verify(clientCodegen)
                .setEnumUnknownDefaultCase(Boolean.parseBoolean(Swift6ClientCodegenOptionsProvider.ENUM_UNKNOWN_DEFAULT_CASE_VALUE));
        verify(clientCodegen).setApiStaticMethod(Boolean.parseBoolean(Swift6ClientCodegenOptionsProvider.API_STATIC_METHOD_VALUE));
        verify(clientCodegen).setCombineDeferred(Boolean.parseBoolean(Swift6ClientCodegenOptionsProvider.COMBINE_DEFERRED_VALUE));
    }
}
