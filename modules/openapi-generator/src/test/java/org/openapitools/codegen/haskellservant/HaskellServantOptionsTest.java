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

package org.openapitools.codegen.haskellservant;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.HaskellServantCodegen;
import org.openapitools.codegen.options.HaskellServantOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class HaskellServantOptionsTest extends AbstractOptionsTest {

    private HaskellServantCodegen clientCodegen = mock(HaskellServantCodegen.class, mockSettings);

    public HaskellServantOptionsTest() {
        super(new HaskellServantOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setModelPackage(HaskellServantOptionsProvider.MODEL_PACKAGE_VALUE);
        verify(clientCodegen).setApiPackage(HaskellServantOptionsProvider.API_PACKAGE_VALUE);
        verify(clientCodegen).setSortParamsByRequiredFlag(Boolean.valueOf(HaskellServantOptionsProvider.SORT_PARAMS_VALUE));
        verify(clientCodegen).setEnumUnknownDefaultCase(Boolean.parseBoolean(HaskellServantOptionsProvider.ENUM_UNKNOWN_DEFAULT_CASE_VALUE));
    }
}
