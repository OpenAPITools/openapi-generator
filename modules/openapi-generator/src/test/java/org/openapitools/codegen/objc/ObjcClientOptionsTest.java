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

package org.openapitools.codegen.objc;

import mockit.Expectations;
import mockit.Tested;
import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.ObjcClientCodegen;
import org.openapitools.codegen.options.ObjcClientOptionsProvider;

public class ObjcClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private ObjcClientCodegen clientCodegen;

    public ObjcClientOptionsTest() {
        super(new ObjcClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setClassPrefix(ObjcClientOptionsProvider.CLASS_PREFIX_VALUE);
            times = 1;
            clientCodegen.setPodName(ObjcClientOptionsProvider.POD_NAME_VALUE);
            times = 1;
            clientCodegen.setPodVersion(ObjcClientOptionsProvider.POD_VERSION_VALUE);
            times = 1;
            clientCodegen.setAuthorName(ObjcClientOptionsProvider.AUTHOR_NAME_VALUE);
            times = 1;
            clientCodegen.setAuthorEmail(ObjcClientOptionsProvider.AUTHOR_EMAIL_VALUE);
            times = 1;
            clientCodegen.setGitRepoURL(ObjcClientOptionsProvider.GIT_REPO_URL_VALUE);
            times = 1;
        }};
    }
}
