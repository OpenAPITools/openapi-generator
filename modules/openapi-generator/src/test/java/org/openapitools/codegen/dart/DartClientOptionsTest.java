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

package org.openapitools.codegen.dart;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.DartClientCodegen;
import org.openapitools.codegen.options.DartClientOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class DartClientOptionsTest extends AbstractOptionsTest {
    private DartClientCodegen clientCodegen = mock(DartClientCodegen.class, mockSettings);

    public DartClientOptionsTest() {
        super(new DartClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setSortParamsByRequiredFlag(Boolean.valueOf(DartClientOptionsProvider.SORT_PARAMS_VALUE));
        verify(clientCodegen).setBrowserClient(Boolean.parseBoolean(DartClientOptionsProvider.BROWSER_CLIENT_VALUE));
        verify(clientCodegen).setPubName(DartClientOptionsProvider.PUB_NAME_VALUE);
        verify(clientCodegen).setPubVersion(DartClientOptionsProvider.PUB_VERSION_VALUE);
        verify(clientCodegen).setPubDescription(DartClientOptionsProvider.PUB_DESCRIPTION_VALUE);
        verify(clientCodegen).setPubAuthor(DartClientOptionsProvider.PUB_AUTHOR_VALUE);
        verify(clientCodegen).setPubAuthorEmail(DartClientOptionsProvider.PUB_AUTHOR_EMAIL_VALUE);
        verify(clientCodegen).setPubHomepage(DartClientOptionsProvider.PUB_HOMEPAGE_VALUE);
        verify(clientCodegen).setSourceFolder(DartClientOptionsProvider.SOURCE_FOLDER_VALUE);
        verify(clientCodegen).setUseEnumExtension(Boolean.parseBoolean(DartClientOptionsProvider.USE_ENUM_EXTENSION));
    }
}
