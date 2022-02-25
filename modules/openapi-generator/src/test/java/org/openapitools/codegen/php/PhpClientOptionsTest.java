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

package org.openapitools.codegen.php;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.PhpClientCodegen;
import org.openapitools.codegen.options.PhpClientOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class PhpClientOptionsTest extends AbstractOptionsTest {
    private PhpClientCodegen clientCodegen = mock(PhpClientCodegen.class, mockSettings);

    public PhpClientOptionsTest() {
        super(new PhpClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setModelPackage(PhpClientOptionsProvider.MODEL_PACKAGE_VALUE);
        verify(clientCodegen).setApiPackage(PhpClientOptionsProvider.API_PACKAGE_VALUE);
        verify(clientCodegen).setSortParamsByRequiredFlag(Boolean.valueOf(PhpClientOptionsProvider.SORT_PARAMS_VALUE));
        verify(clientCodegen).setParameterNamingConvention(PhpClientOptionsProvider.VARIABLE_NAMING_CONVENTION_VALUE);
        verify(clientCodegen).setInvokerPackage(PhpClientOptionsProvider.INVOKER_PACKAGE_VALUE);
        verify(clientCodegen).setPackageName(PhpClientOptionsProvider.PACKAGE_NAME_VALUE);
        verify(clientCodegen).setSrcBasePath(PhpClientOptionsProvider.SRC_BASE_PATH_VALUE);
        verify(clientCodegen).setArtifactVersion(PhpClientOptionsProvider.ARTIFACT_VERSION_VALUE);
        verify(clientCodegen).setEnumUnknownDefaultCase(Boolean.parseBoolean(PhpClientOptionsProvider.ENUM_UNKNOWN_DEFAULT_CASE_VALUE));
    }
}
