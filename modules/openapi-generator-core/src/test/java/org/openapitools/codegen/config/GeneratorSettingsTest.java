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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertThrows;

public class GeneratorSettingsTest {

    @Test
    public void withInjectModelVendorExtensionAppendsRepeatedKeyValuesInOrder() {
        GeneratorSettings settings = GeneratorSettings.newBuilder()
                .withGeneratorName("spring")
                .withInjectModelVendorExtension("Pet.x-class-extra-annotation", "@Foo")
                .withInjectModelVendorExtension("Pet.x-class-extra-annotation", "@Bar")
                .build();

        assertEquals(settings.getInjectModelVendorExtensions().get("Pet.x-class-extra-annotation"), List.of("@Foo", "@Bar"));
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
                List.of("@Foo", "@Bar", "@Baz"));
    }

    @Test
    public void withInjectOperationVendorExtensionKeepsDistinctKeysIndependent() {
        GeneratorSettings settings = GeneratorSettings.newBuilder()
                .withGeneratorName("spring")
                .withInjectOperationVendorExtension("addPet.x-operation-extra-annotation", "@Foo")
                .withInjectOperationVendorExtension("addPet.orgId.x-field-extra-annotation", "@Bar")
                .build();

        assertEquals(settings.getInjectOperationVendorExtensions().get("addPet.x-operation-extra-annotation"), List.of("@Foo"));
        assertEquals(settings.getInjectOperationVendorExtensions().get("addPet.orgId.x-field-extra-annotation"), List.of("@Bar"));
    }

    @Test
    public void getInjectModelVendorExtensionsListIsUnmodifiable() {
        GeneratorSettings settings = GeneratorSettings.newBuilder()
                .withGeneratorName("spring")
                .withInjectModelVendorExtension("Pet.x-class-extra-annotation", "@Foo")
                .build();

        List<String> values = settings.getInjectModelVendorExtensions().get("Pet.x-class-extra-annotation");
        assertThrows(UnsupportedOperationException.class, () -> values.add("@Bar"));
    }

    @Test
    public void copyingBuilderDoesNotLeakMutationsBetweenSettingsInstances() {
        GeneratorSettings original = GeneratorSettings.newBuilder()
                .withGeneratorName("spring")
                .withInjectModelVendorExtension("Pet.x-class-extra-annotation", "@Foo")
                .build();

        // Derive a second settings instance from the first (as GenerateBatch does when several
        // configurators share a base config) and append another value to the same key.
        GeneratorSettings.newBuilder(original)
                .withInjectModelVendorExtension("Pet.x-class-extra-annotation", "@Bar")
                .build();

        // The original, already-built settings must be unaffected by the copy's mutation.
        assertEquals(original.getInjectModelVendorExtensions().get("Pet.x-class-extra-annotation"), List.of("@Foo"));
    }

    @Test
    public void withInjectModelVendorExtensionsBulkSetterDoesNotShareListsWithCaller() {
        Map<String, List<String>> extensions = new HashMap<>();
        extensions.put("Pet.x-class-extra-annotation", new ArrayList<>(List.of("@Foo")));

        GeneratorSettings settings = GeneratorSettings.newBuilder()
                .withGeneratorName("spring")
                .withInjectModelVendorExtensions(extensions)
                .build();

        // Mutating the caller's map/list after building must not affect the built settings.
        extensions.get("Pet.x-class-extra-annotation").add("@Bar");

        assertEquals(settings.getInjectModelVendorExtensions().get("Pet.x-class-extra-annotation"), List.of("@Foo"));
    }
}
