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

import com.fasterxml.jackson.core.Version;
import com.fasterxml.jackson.databind.Module;

/**
 * Registers {@link VendorExtensionsMapDeserializerModifier} so config-file deserialization accepts
 * either a scalar string or a list of strings for each entry of a {@code Map<String, List<String>>}
 * property, such as {@code injectModelVendorExtensions}/{@code injectOperationVendorExtensions}.
 */
class VendorExtensionsCompatModule extends Module {

    @Override
    public String getModuleName() {
        return "VendorExtensionsCompatModule";
    }

    @Override
    public Version version() {
        return Version.unknownVersion();
    }

    @Override
    public void setupModule(SetupContext context) {
        context.addBeanDeserializerModifier(new VendorExtensionsMapDeserializerModifier());
    }
}
