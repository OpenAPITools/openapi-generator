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

package org.openapitools.codegen.scalaakka;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.ScalaAkkaClientCodegen;
import org.openapitools.codegen.options.ScalaAkkaClientOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class ScalaAkkaClientOptionsTest extends AbstractOptionsTest {
    private ScalaAkkaClientCodegen clientCodegen = mock(ScalaAkkaClientCodegen.class, mockSettings);

    public ScalaAkkaClientOptionsTest() {
        super(new ScalaAkkaClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setModelPackage(ScalaAkkaClientOptionsProvider.MODEL_PACKAGE_VALUE);
        verify(clientCodegen).setApiPackage(ScalaAkkaClientOptionsProvider.API_PACKAGE_VALUE);
        verify(clientCodegen).setSortParamsByRequiredFlag(Boolean.valueOf(ScalaAkkaClientOptionsProvider.SORT_PARAMS_VALUE));
        verify(clientCodegen).setSourceFolder(ScalaAkkaClientOptionsProvider.SOURCE_FOLDER_VALUE);
        verify(clientCodegen).setPrependFormOrBodyParameters(Boolean.valueOf(ScalaAkkaClientOptionsProvider.PREPEND_FORM_OR_BODY_PARAMETERS_VALUE));
        verify(clientCodegen).setMainPackage(ScalaAkkaClientOptionsProvider.MAIN_PACKAGE_VALUE);
        verify(clientCodegen).setModelPropertyNaming(ScalaAkkaClientOptionsProvider.MODEL_PROPERTY_NAMING);
    }
}
