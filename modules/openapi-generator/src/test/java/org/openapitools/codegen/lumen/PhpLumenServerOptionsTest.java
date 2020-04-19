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

package org.openapitools.codegen.lumen;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.PhpLumenServerCodegen;
import org.openapitools.codegen.options.PhpLumenServerOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class PhpLumenServerOptionsTest extends AbstractOptionsTest {
    private PhpLumenServerCodegen clientCodegen = mock(PhpLumenServerCodegen.class, mockSettings);

    public PhpLumenServerOptionsTest() {
        super(new PhpLumenServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setSortParamsByRequiredFlag(Boolean.valueOf(PhpLumenServerOptionsProvider.SORT_PARAMS_VALUE));
        verify(clientCodegen).setParameterNamingConvention(PhpLumenServerOptionsProvider.VARIABLE_NAMING_CONVENTION_VALUE);
        verify(clientCodegen).setModelPackage(PhpLumenServerOptionsProvider.MODEL_PACKAGE_VALUE);
        verify(clientCodegen).setApiPackage(PhpLumenServerOptionsProvider.API_PACKAGE_VALUE);
        verify(clientCodegen).setInvokerPackage(PhpLumenServerOptionsProvider.INVOKER_PACKAGE_VALUE);
        verify(clientCodegen).setPackageName(PhpLumenServerOptionsProvider.PACKAGE_NAME_VALUE);
        verify(clientCodegen).setSrcBasePath(PhpLumenServerOptionsProvider.SRC_BASE_PATH_VALUE);
        verify(clientCodegen).setArtifactVersion(PhpLumenServerOptionsProvider.ARTIFACT_VERSION_VALUE);
    }
}
