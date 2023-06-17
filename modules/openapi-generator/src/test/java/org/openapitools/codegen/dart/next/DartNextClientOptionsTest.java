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

package org.openapitools.codegen.dart.next;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.DartNextClientCodegen;
import org.openapitools.codegen.options.DartNextClientOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class DartNextClientOptionsTest extends AbstractOptionsTest {
    private DartNextClientCodegen clientCodegen = mock(DartNextClientCodegen.class, mockSettings);

    public DartNextClientOptionsTest() {
        super(new DartNextClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setSortParamsByRequiredFlag(Boolean.valueOf(DartNextClientOptionsProvider.SORT_PARAMS_VALUE));
        verify(clientCodegen).setPubLibrary(DartNextClientOptionsProvider.PUB_LIBRARY_VALUE);
        verify(clientCodegen).setPubName(DartNextClientOptionsProvider.PUB_NAME_VALUE);
        verify(clientCodegen).setPubVersion(DartNextClientOptionsProvider.PUB_VERSION_VALUE);
        verify(clientCodegen).setPubDescription(DartNextClientOptionsProvider.PUB_DESCRIPTION_VALUE);
        verify(clientCodegen).setPubAuthor(DartNextClientOptionsProvider.PUB_AUTHOR_VALUE);
        verify(clientCodegen).setPubAuthorEmail(DartNextClientOptionsProvider.PUB_AUTHOR_EMAIL_VALUE);
        verify(clientCodegen).setPubHomepage(DartNextClientOptionsProvider.PUB_HOMEPAGE_VALUE);
        verify(clientCodegen).setPubRepository(DartNextClientOptionsProvider.PUB_REPOSITORY_VALUE);
        verify(clientCodegen).setPubPublishTo(DartNextClientOptionsProvider.PUB_PUBLISH_TO_VALUE);
        verify(clientCodegen).setSourceFolder(DartNextClientOptionsProvider.SOURCE_FOLDER_VALUE);
        verify(clientCodegen).setUseEnumExtension(Boolean.parseBoolean(DartNextClientOptionsProvider.USE_ENUM_EXTENSION));
        verify(clientCodegen).setDateLibrary(DartNextClientCodegen.DATE_LIBRARY_DEFAULT);
        verify(clientCodegen).setLibrary(DartNextClientCodegen.NETWORKING_LIBRARY_DEFAULT);
        verify(clientCodegen).setSerializationLibrary(DartNextClientCodegen.SERIALIZATION_LIBRARY_DEFAULT);
        verify(clientCodegen).setEnumUnknownDefaultCase(Boolean.parseBoolean(DartNextClientOptionsProvider.ENUM_UNKNOWN_DEFAULT_CASE_VALUE));
    }
}
