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

package org.openapitools.codegen.slim;

import mockit.Expectations;
import mockit.Tested;
import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.PhpSlimServerCodegen;
import org.openapitools.codegen.options.PhpSlimServerOptionsProvider;

public class PhpSlimServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private PhpSlimServerCodegen clientCodegen;

    public PhpSlimServerOptionsTest() {
        super(new PhpSlimServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(PhpSlimServerOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(PhpSlimServerOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setParameterNamingConvention(PhpSlimServerOptionsProvider.VARIABLE_NAMING_CONVENTION_VALUE);
            times = 1;
            clientCodegen.setInvokerPackage(PhpSlimServerOptionsProvider.INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setPackageName(PhpSlimServerOptionsProvider.PACKAGE_NAME_VALUE);
            times = 1;
            clientCodegen.setSrcBasePath(PhpSlimServerOptionsProvider.SRC_BASE_PATH_VALUE);
            times = 1;
            clientCodegen.setArtifactVersion(PhpSlimServerOptionsProvider.ARTIFACT_VERSION_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(PhpSlimServerOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
