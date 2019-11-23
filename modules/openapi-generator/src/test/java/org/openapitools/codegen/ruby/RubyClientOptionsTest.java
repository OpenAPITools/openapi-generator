/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.ruby;

import mockit.Expectations;
import mockit.Tested;
import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.RubyClientCodegen;
import org.openapitools.codegen.options.RubyClientOptionsProvider;

public class RubyClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private RubyClientCodegen clientCodegen;

    public RubyClientOptionsTest() {
        super(new RubyClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setGemName(RubyClientOptionsProvider.GEM_NAME_VALUE);
            times = 1;
            clientCodegen.setModuleName(RubyClientOptionsProvider.MODULE_NAME_VALUE);
            times = 1;
            clientCodegen.setGemVersion(RubyClientOptionsProvider.GEM_VERSION_VALUE);
            times = 1;
            clientCodegen.setGemLicense(RubyClientOptionsProvider.GEM_LICENSE_VALUE);
            times = 1;
            clientCodegen.setGemRequiredRubyVersion(RubyClientOptionsProvider.GEM_REQUIRED_RUBY_VERSION_VALUE);
            times = 1;
            clientCodegen.setGemHomepage(RubyClientOptionsProvider.GEM_HOMEPAGE_VALUE);
            times = 1;
            clientCodegen.setGemDescription(RubyClientOptionsProvider.GEM_DESCRIPTION_VALUE);
            times = 1;
            clientCodegen.setGemSummary(RubyClientOptionsProvider.GEM_SUMMARY_VALUE);
            times = 1;
            clientCodegen.setGemAuthor(RubyClientOptionsProvider.GEM_AUTHOR_VALUE);
            times = 1;
            clientCodegen.setGemAuthorEmail(RubyClientOptionsProvider.GEM_AUTHOR_EMAIL_VALUE);
            times = 1;

        }};
    }
}
