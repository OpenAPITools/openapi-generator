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

package org.openapitools.codegen.lumen;

import mockit.Expectations;
import mockit.Tested;
import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.PhpLumenServerCodegen;
import org.openapitools.codegen.options.PhpLumenServerOptionsProvider;

public class PhpLumenServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private PhpLumenServerCodegen clientCodegen;

    public PhpLumenServerOptionsTest() {
        super(new PhpLumenServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(PhpLumenServerOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setParameterNamingConvention(PhpLumenServerOptionsProvider.VARIABLE_NAMING_CONVENTION_VALUE);
            clientCodegen.setModelPackage(PhpLumenServerOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(PhpLumenServerOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            times = 1;
            clientCodegen.setInvokerPackage(PhpLumenServerOptionsProvider.INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setPackageName(PhpLumenServerOptionsProvider.PACKAGE_NAME_VALUE);
            times = 1;
            clientCodegen.setSrcBasePath(PhpLumenServerOptionsProvider.SRC_BASE_PATH_VALUE);
            times = 1;
            clientCodegen.setArtifactVersion(PhpLumenServerOptionsProvider.ARTIFACT_VERSION_VALUE);
            times = 1;
        }};
    }
}
