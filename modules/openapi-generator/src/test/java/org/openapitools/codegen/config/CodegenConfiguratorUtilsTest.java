/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.config;

import org.testng.annotations.Test;

import java.util.Arrays;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

/**
 * Unlike most CLI/Maven key-value-pair options, {@code --inject-model-vendor-extensions} and
 * {@code --inject-operation-vendor-extensions} values are commonly annotation literals containing
 * their own commas (e.g. {@code @Size(min = 1, max = 100)}). These tests confirm that each option
 * occurrence is parsed as exactly one key=value pair (split only on the first '='), so such commas
 * are kept literally instead of being mistaken for a separator between injection targets.
 */
public class CodegenConfiguratorUtilsTest {

    @Test
    public void injectModelVendorExtensionsKvpKeepsCommasInValueLiteral() {
        CodegenConfigurator configurator = mock(CodegenConfigurator.class);
        CodegenConfiguratorUtils.applyInjectModelVendorExtensionsKvp(
                "Pet.x-class-extra-annotation=@Size(min = 1, max = 100)", configurator);

        verify(configurator).addInjectModelVendorExtension("Pet.x-class-extra-annotation", "@Size(min = 1, max = 100)");
        verifyNoMoreInteractions(configurator);
    }

    @Test
    public void injectOperationVendorExtensionsKvpKeepsCommasInValueLiteral() {
        CodegenConfigurator configurator = mock(CodegenConfigurator.class);
        CodegenConfiguratorUtils.applyInjectOperationVendorExtensionsKvp(
                "addPet.orgId.x-field-extra-annotation=@Size(min = 1, max = 100) @Deprecated", configurator);

        verify(configurator).addInjectOperationVendorExtension(
                "addPet.orgId.x-field-extra-annotation", "@Size(min = 1, max = 100) @Deprecated");
        verifyNoMoreInteractions(configurator);
    }

    @Test
    public void injectOperationVendorExtensionsKvpListSupportsMultipleOccurrences() {
        CodegenConfigurator configurator = mock(CodegenConfigurator.class);
        CodegenConfiguratorUtils.applyInjectOperationVendorExtensionsKvpList(
                Arrays.asList(
                        "addPet.x-operation-extra-annotation=@Deprecated",
                        "addPet.orgId.x-field-extra-annotation=@Size(min = 1, max = 100)"),
                configurator);

        verify(configurator).addInjectOperationVendorExtension("addPet.x-operation-extra-annotation", "@Deprecated");
        verify(configurator).addInjectOperationVendorExtension(
                "addPet.orgId.x-field-extra-annotation", "@Size(min = 1, max = 100)");
        verifyNoMoreInteractions(configurator);
    }

    @Test
    public void injectModelVendorExtensionsKvpIgnoresEntryWithoutEquals() {
        CodegenConfigurator configurator = mock(CodegenConfigurator.class);
        CodegenConfiguratorUtils.applyInjectModelVendorExtensionsKvp("not-a-kvp", configurator);

        verifyNoMoreInteractions(configurator);
    }
}
