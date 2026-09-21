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

import com.fasterxml.jackson.databind.BeanDescription;
import com.fasterxml.jackson.databind.DeserializationConfig;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.deser.BeanDeserializerModifier;
import com.fasterxml.jackson.databind.type.MapType;

/**
 * Makes any {@code Map<String, List<String>>} property (regardless of how Jackson chooses to bind
 * it: field, getter/setter, or builder method) accept either a scalar string or a list of strings
 * per map entry, via {@link VendorExtensionsMapDeserializer}. This keeps config files written before
 * {@code injectModelVendorExtensions}/{@code injectOperationVendorExtensions} values became lists
 * (e.g. {@code x-setter-visibility: private}) working unchanged. Registered on the config-file
 * {@code ObjectMapper} in {@link CodegenConfigurator}.
 */
class VendorExtensionsMapDeserializerModifier extends BeanDeserializerModifier {

    @Override
    public JsonDeserializer<?> modifyMapDeserializer(DeserializationConfig config, MapType type,
                                                      BeanDescription beanDesc, JsonDeserializer<?> deserializer) {
        JavaType contentType = type.getContentType();
        if (contentType != null
                && contentType.hasRawClass(java.util.List.class)
                && contentType.containedTypeCount() > 0
                && contentType.containedType(0).hasRawClass(String.class)) {
            return new VendorExtensionsMapDeserializer();
        }
        return deserializer;
    }
}
