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

package org.openapitools.codegen.dartdio;

import mockit.Expectations;
import mockit.Tested;
import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.DartDioClientCodegen;
import org.openapitools.codegen.options.DartDioClientOptionsProvider;

public class DartDioClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private DartDioClientCodegen clientCodegen;

    public DartDioClientOptionsTest() {
        super(new DartDioClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(DartDioClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setBrowserClient(Boolean.valueOf(DartDioClientOptionsProvider.BROWSER_CLIENT_VALUE));
            times = 1;
            clientCodegen.setPubName(DartDioClientOptionsProvider.PUB_NAME_VALUE);
            times = 1;
            clientCodegen.setPubVersion(DartDioClientOptionsProvider.PUB_VERSION_VALUE);
            times = 1;
            clientCodegen.setPubDescription(DartDioClientOptionsProvider.PUB_DESCRIPTION_VALUE);
            times = 1;
            clientCodegen.setPubAuthor(DartDioClientOptionsProvider.PUB_AUTHOR_VALUE);
            times = 1;
            clientCodegen.setPubAuthorEmail(DartDioClientOptionsProvider.PUB_AUTHOR_EMAIL_VALUE);
            times = 1;
            clientCodegen.setPubHomepage(DartDioClientOptionsProvider.PUB_HOMEPAGE_VALUE);
            times = 1;
            clientCodegen.setSourceFolder(DartDioClientOptionsProvider.SOURCE_FOLDER_VALUE);
            times = 1;
            clientCodegen.setUseEnumExtension(Boolean.valueOf(DartDioClientOptionsProvider.USE_ENUM_EXTENSION));
            times = 1;
            clientCodegen.setDateLibrary(DartDioClientOptionsProvider.DATE_LIBRARY);
            times = 1;
            clientCodegen.setNullableFields(Boolean.valueOf(DartDioClientOptionsProvider.NULLABLE_FIELDS));
            times = 1;
        }};
    }
}
