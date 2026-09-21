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

import static org.testng.Assert.assertEquals;

public class GeneratorSettingsTest {

    @Test
    public void withInjectModelVendorExtensionAppendsRepeatedKeyValuesInOrder() {
        GeneratorSettings settings = GeneratorSettings.newBuilder()
                .withGeneratorName("spring")
                .withInjectModelVendorExtension("Pet.x-class-extra-annotation", "@Foo")
                .withInjectModelVendorExtension("Pet.x-class-extra-annotation", "@Bar")
                .build();

        assertEquals(settings.getInjectModelVendorExtensions().get("Pet.x-class-extra-annotation"), "@Foo @Bar");
    }

    @Test
    public void withInjectOperationVendorExtensionAppendsRepeatedKeyValuesInOrder() {
        GeneratorSettings settings = GeneratorSettings.newBuilder()
                .withGeneratorName("spring")
                .withInjectOperationVendorExtension("addPet.x-operation-extra-annotation", "@Foo")
                .withInjectOperationVendorExtension("addPet.x-operation-extra-annotation", "@Bar")
                .withInjectOperationVendorExtension("addPet.x-operation-extra-annotation", "@Baz")
                .build();

        assertEquals(settings.getInjectOperationVendorExtensions().get("addPet.x-operation-extra-annotation"),
                "@Foo @Bar @Baz");
    }

    @Test
    public void withInjectOperationVendorExtensionKeepsDistinctKeysIndependent() {
        GeneratorSettings settings = GeneratorSettings.newBuilder()
                .withGeneratorName("spring")
                .withInjectOperationVendorExtension("addPet.x-operation-extra-annotation", "@Foo")
                .withInjectOperationVendorExtension("addPet.orgId.x-field-extra-annotation", "@Bar")
                .build();

        assertEquals(settings.getInjectOperationVendorExtensions().get("addPet.x-operation-extra-annotation"), "@Foo");
        assertEquals(settings.getInjectOperationVendorExtensions().get("addPet.orgId.x-field-extra-annotation"), "@Bar");
    }
}
