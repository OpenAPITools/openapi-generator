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

package org.openapitools.codegen.slim;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.PhpSlimServerCodegen;
import org.openapitools.codegen.options.PhpSlimServerOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class PhpSlimServerOptionsTest extends AbstractOptionsTest {
    private PhpSlimServerCodegen clientCodegen = mock(PhpSlimServerCodegen.class, mockSettings);

    public PhpSlimServerOptionsTest() {
        super(new PhpSlimServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setModelPackage(PhpSlimServerOptionsProvider.MODEL_PACKAGE_VALUE);
        verify(clientCodegen).setApiPackage(PhpSlimServerOptionsProvider.API_PACKAGE_VALUE);
        verify(clientCodegen).setParameterNamingConvention(PhpSlimServerOptionsProvider.VARIABLE_NAMING_CONVENTION_VALUE);
        verify(clientCodegen).setInvokerPackage(PhpSlimServerOptionsProvider.INVOKER_PACKAGE_VALUE);
        verify(clientCodegen).setPackageName(PhpSlimServerOptionsProvider.PACKAGE_NAME_VALUE);
        verify(clientCodegen).setSrcBasePath(PhpSlimServerOptionsProvider.SRC_BASE_PATH_VALUE);
        verify(clientCodegen).setArtifactVersion(PhpSlimServerOptionsProvider.ARTIFACT_VERSION_VALUE);
        verify(clientCodegen).setSortParamsByRequiredFlag(Boolean.valueOf(PhpSlimServerOptionsProvider.SORT_PARAMS_VALUE));
        verify(clientCodegen).setEnumUnknownDefaultCase(Boolean.parseBoolean(PhpSlimServerOptionsProvider.ENUM_UNKNOWN_DEFAULT_CASE_VALUE));
    }
}
