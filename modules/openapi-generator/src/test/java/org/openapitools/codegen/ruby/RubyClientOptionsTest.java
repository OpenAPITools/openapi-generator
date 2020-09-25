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

package org.openapitools.codegen.ruby;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.RubyClientCodegen;
import org.openapitools.codegen.options.RubyClientOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class RubyClientOptionsTest extends AbstractOptionsTest {
    private RubyClientCodegen clientCodegen = mock(RubyClientCodegen.class, mockSettings);

    public RubyClientOptionsTest() {
        super(new RubyClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setGemName(RubyClientOptionsProvider.GEM_NAME_VALUE);
        verify(clientCodegen).setModuleName(RubyClientOptionsProvider.MODULE_NAME_VALUE);
        verify(clientCodegen).setGemVersion(RubyClientOptionsProvider.GEM_VERSION_VALUE);
        verify(clientCodegen).setGemLicense(RubyClientOptionsProvider.GEM_LICENSE_VALUE);
        verify(clientCodegen).setGemRequiredRubyVersion(RubyClientOptionsProvider.GEM_REQUIRED_RUBY_VERSION_VALUE);
        verify(clientCodegen).setGemHomepage(RubyClientOptionsProvider.GEM_HOMEPAGE_VALUE);
        verify(clientCodegen).setGemDescription(RubyClientOptionsProvider.GEM_DESCRIPTION_VALUE);
        verify(clientCodegen).setGemSummary(RubyClientOptionsProvider.GEM_SUMMARY_VALUE);
        verify(clientCodegen).setGemAuthor(RubyClientOptionsProvider.GEM_AUTHOR_VALUE);
        verify(clientCodegen).setGemAuthorEmail(RubyClientOptionsProvider.GEM_AUTHOR_EMAIL_VALUE);
    }
}
