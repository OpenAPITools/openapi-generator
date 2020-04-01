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

package org.openapitools.codegen.scalahttpclient;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.ScalaHttpClientCodegen;
import org.openapitools.codegen.options.ScalaHttpClientOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class ScalaHttpClientOptionsTest extends AbstractOptionsTest {

    private ScalaHttpClientCodegen clientCodegen = mock(ScalaHttpClientCodegen.class, mockSettings);

    public ScalaHttpClientOptionsTest() {
        super(new ScalaHttpClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setModelPackage(ScalaHttpClientOptionsProvider.MODEL_PACKAGE_VALUE);
        verify(clientCodegen).setApiPackage(ScalaHttpClientOptionsProvider.API_PACKAGE_VALUE);
        verify(clientCodegen).setSortParamsByRequiredFlag(Boolean.valueOf(ScalaHttpClientOptionsProvider.SORT_PARAMS_VALUE));
        verify(clientCodegen).setModelPropertyNaming(ScalaHttpClientOptionsProvider.MODEL_PROPERTY_NAMING);
        verify(clientCodegen).setSourceFolder(ScalaHttpClientOptionsProvider.SOURCE_FOLDER_VALUE);
        verify(clientCodegen).setPrependFormOrBodyParameters(Boolean.valueOf(ScalaHttpClientOptionsProvider.PREPEND_FORM_OR_BODY_PARAMETERS_VALUE));
        verify(clientCodegen).setDateLibrary(ScalaHttpClientOptionsProvider.DATE_LIBRARY,false);
    }
}
