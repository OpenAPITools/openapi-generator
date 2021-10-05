/*
 * Copyright 2021 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.dart.dio;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.DartDioNextClientCodegen;
import org.openapitools.codegen.options.DartDioNextClientOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class DartDioNextClientOptionsTest extends AbstractOptionsTest {
    private DartDioNextClientCodegen clientCodegen = mock(DartDioNextClientCodegen.class, mockSettings);

    public DartDioNextClientOptionsTest() {
        super(new DartDioNextClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setSortParamsByRequiredFlag(Boolean.valueOf(DartDioNextClientOptionsProvider.SORT_PARAMS_VALUE));
        verify(clientCodegen).setPubLibrary(DartDioNextClientOptionsProvider.PUB_LIBRARY_VALUE);
        verify(clientCodegen).setPubName(DartDioNextClientOptionsProvider.PUB_NAME_VALUE);
        verify(clientCodegen).setPubVersion(DartDioNextClientOptionsProvider.PUB_VERSION_VALUE);
        verify(clientCodegen).setPubDescription(DartDioNextClientOptionsProvider.PUB_DESCRIPTION_VALUE);
        verify(clientCodegen).setPubAuthor(DartDioNextClientOptionsProvider.PUB_AUTHOR_VALUE);
        verify(clientCodegen).setPubAuthorEmail(DartDioNextClientOptionsProvider.PUB_AUTHOR_EMAIL_VALUE);
        verify(clientCodegen).setPubHomepage(DartDioNextClientOptionsProvider.PUB_HOMEPAGE_VALUE);
        verify(clientCodegen).setSourceFolder(DartDioNextClientOptionsProvider.SOURCE_FOLDER_VALUE);
        verify(clientCodegen).setUseEnumExtension(Boolean.parseBoolean(DartDioNextClientOptionsProvider.USE_ENUM_EXTENSION));
        verify(clientCodegen).setDateLibrary(DartDioNextClientCodegen.DATE_LIBRARY_DEFAULT);
        verify(clientCodegen).setDioLibrary(DartDioNextClientCodegen.DIO_LIBRARY_DEFAULT);
        verify(clientCodegen).setLibrary(DartDioNextClientCodegen.SERIALIZATION_LIBRARY_DEFAULT);
    }
}
