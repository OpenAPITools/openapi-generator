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

package org.openapitools.codegen.slim4;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.PhpSlim4ServerCodegen;
import org.openapitools.codegen.options.PhpSlim4ServerOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class PhpSlim4ServerOptionsTest extends AbstractOptionsTest {
    private PhpSlim4ServerCodegen clientCodegen = mock(PhpSlim4ServerCodegen.class, mockSettings);

    public PhpSlim4ServerOptionsTest() {
        super(new PhpSlim4ServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setModelPackage(PhpSlim4ServerOptionsProvider.MODEL_PACKAGE_VALUE);
        verify(clientCodegen).setApiPackage(PhpSlim4ServerOptionsProvider.API_PACKAGE_VALUE);
        verify(clientCodegen).setParameterNamingConvention(PhpSlim4ServerOptionsProvider.VARIABLE_NAMING_CONVENTION_VALUE);
        verify(clientCodegen).setInvokerPackage(PhpSlim4ServerOptionsProvider.INVOKER_PACKAGE_VALUE);
        verify(clientCodegen).setPackageName(PhpSlim4ServerOptionsProvider.PACKAGE_NAME_VALUE);
        verify(clientCodegen).setSrcBasePath(PhpSlim4ServerOptionsProvider.SRC_BASE_PATH_VALUE);
        verify(clientCodegen).setArtifactVersion(PhpSlim4ServerOptionsProvider.ARTIFACT_VERSION_VALUE);
        verify(clientCodegen).setSortParamsByRequiredFlag(Boolean.valueOf(PhpSlim4ServerOptionsProvider.SORT_PARAMS_VALUE));
        verify(clientCodegen).setPsr7Implementation(PhpSlim4ServerOptionsProvider.PSR7_IMPLEMENTATION_VALUE);
    }
}

