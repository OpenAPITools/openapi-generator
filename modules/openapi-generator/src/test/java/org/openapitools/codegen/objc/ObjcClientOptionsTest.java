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

package org.openapitools.codegen.objc;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.ObjcClientCodegen;
import org.openapitools.codegen.options.ObjcClientOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class ObjcClientOptionsTest extends AbstractOptionsTest {
    private ObjcClientCodegen clientCodegen = mock(ObjcClientCodegen.class, mockSettings);

    public ObjcClientOptionsTest() {
        super(new ObjcClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setClassPrefix(ObjcClientOptionsProvider.CLASS_PREFIX_VALUE);
        verify(clientCodegen).setPodName(ObjcClientOptionsProvider.POD_NAME_VALUE);
        verify(clientCodegen).setPodVersion(ObjcClientOptionsProvider.POD_VERSION_VALUE);
        verify(clientCodegen).setAuthorName(ObjcClientOptionsProvider.AUTHOR_NAME_VALUE);
        verify(clientCodegen).setAuthorEmail(ObjcClientOptionsProvider.AUTHOR_EMAIL_VALUE);
        verify(clientCodegen).setGitRepoURL(ObjcClientOptionsProvider.GIT_REPO_URL_VALUE);
    }
}
