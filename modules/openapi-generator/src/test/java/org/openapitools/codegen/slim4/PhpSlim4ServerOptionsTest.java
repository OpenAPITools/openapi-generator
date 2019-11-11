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

package org.openapitools.codegen.slim4;

import mockit.Expectations;
import mockit.Tested;
import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.PhpSlim4ServerCodegen;
import org.openapitools.codegen.options.PhpSlim4ServerOptionsProvider;

public class PhpSlim4ServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private PhpSlim4ServerCodegen clientCodegen;

    public PhpSlim4ServerOptionsTest() {
        super(new PhpSlim4ServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(PhpSlim4ServerOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(PhpSlim4ServerOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setParameterNamingConvention(PhpSlim4ServerOptionsProvider.VARIABLE_NAMING_CONVENTION_VALUE);
            times = 1;
            clientCodegen.setInvokerPackage(PhpSlim4ServerOptionsProvider.INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setPackageName(PhpSlim4ServerOptionsProvider.PACKAGE_NAME_VALUE);
            times = 1;
            clientCodegen.setSrcBasePath(PhpSlim4ServerOptionsProvider.SRC_BASE_PATH_VALUE);
            times = 1;
            clientCodegen.setArtifactVersion(PhpSlim4ServerOptionsProvider.ARTIFACT_VERSION_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(PhpSlim4ServerOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setPsr7Implementation(PhpSlim4ServerOptionsProvider.PSR7_IMPLEMENTATION_VALUE);
            times = 1;
        }};
    }
}

