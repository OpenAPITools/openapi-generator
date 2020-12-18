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
import org.openapitools.codegen.languages.PhpSymfony5ServerCodegen;
import org.openapitools.codegen.options.PhpSymfony5ServerOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class PhpSymfony5ServerOptionsTest extends AbstractOptionsTest {
    private PhpSymfony5ServerCodegen codegen = mock(PhpSymfony5ServerCodegen.class, mockSettings);

    public PhpSymfony5ServerOptionsTest() {
        super(new PhpSymfony5ServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return codegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(codegen).setModelPackage(PhpSymfony5ServerOptionsProvider.MODEL_PACKAGE_VALUE);
        verify(codegen).setApiPackage(PhpSymfony5ServerOptionsProvider.API_PACKAGE_VALUE);
        verify(codegen).setParameterNamingConvention(PhpSymfony5ServerOptionsProvider.VARIABLE_NAMING_CONVENTION_VALUE);
        verify(codegen).setInvokerPackage(PhpSymfony5ServerOptionsProvider.INVOKER_PACKAGE_VALUE);
        verify(codegen).setPackageName(PhpSymfony5ServerOptionsProvider.PACKAGE_NAME_VALUE);
        verify(codegen).setSrcBasePath(PhpSymfony5ServerOptionsProvider.SRC_BASE_PATH_VALUE);
        verify(codegen).setArtifactVersion(PhpSymfony5ServerOptionsProvider.ARTIFACT_VERSION_VALUE);
        verify(codegen).setSortParamsByRequiredFlag(Boolean.valueOf(PhpSymfony5ServerOptionsProvider.SORT_PARAMS_VALUE));
        verify(codegen).setHideGenerationTimestamp(Boolean.valueOf(PhpSymfony5ServerOptionsProvider.HIDE_GENERATION_TIMESTAMP_VALUE));
        verify(codegen).setBundleName(PhpSymfony5ServerOptionsProvider.BUNDLE_NAME_VALUE);
        verify(codegen).setBundleAlias(PhpSymfony5ServerOptionsProvider.BUNDLE_ALIAS_VALUE);
        verify(codegen).setComposerVendorName(PhpSymfony5ServerOptionsProvider.COMPOSER_VENDOR_NAME_VALUE);
        verify(codegen).setComposerProjectName(PhpSymfony5ServerOptionsProvider.COMPOSER_PROJECT_NAME_VALUE);
    }
}

