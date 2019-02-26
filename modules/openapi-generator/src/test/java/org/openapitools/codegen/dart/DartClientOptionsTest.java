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

package org.openapitools.codegen.dart;

import mockit.Expectations;
import mockit.Tested;
import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.DartClientCodegen;
import org.openapitools.codegen.options.DartClientOptionsProvider;

public class DartClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private DartClientCodegen clientCodegen;

    public DartClientOptionsTest() {
        super(new DartClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(DartClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setBrowserClient(Boolean.valueOf(DartClientOptionsProvider.BROWSER_CLIENT_VALUE));
            times = 1;
            clientCodegen.setPubName(DartClientOptionsProvider.PUB_NAME_VALUE);
            times = 1;
            clientCodegen.setPubVersion(DartClientOptionsProvider.PUB_VERSION_VALUE);
            times = 1;
            clientCodegen.setPubDescription(DartClientOptionsProvider.PUB_DESCRIPTION_VALUE);
            times = 1;
            clientCodegen.setSourceFolder(DartClientOptionsProvider.SOURCE_FOLDER_VALUE);
            times = 1;
            clientCodegen.setUseEnumExtension(Boolean.valueOf(DartClientOptionsProvider.USE_ENUM_EXTENSION));
            times = 1;
        }};
    }
}
