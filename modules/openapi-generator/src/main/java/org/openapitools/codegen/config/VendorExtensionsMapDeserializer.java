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

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonNode;

import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Deserializes a {@code Map<String, List<String>>} whose values may be authored either as a plain
 * scalar string per key (the shape used by config files written before injected vendor extension
 * values became lists, e.g. {@code x-setter-visibility: private}) or as a JSON/YAML array of
 * strings (the current shape, allowing multiple values per key). Either shape always yields a
 * {@code List<String>} value, so config files predating this change keep working unchanged.
 * <p>
 * Implemented as a full map deserializer, rather than a value-only content deserializer, because
 * it is registered via {@link VendorExtensionsMapDeserializerModifier} for any
 * {@code Map<String, List<String>>} type regardless of how Jackson resolves the containing
 * property (field, getter/setter, or builder method).
 */
class VendorExtensionsMapDeserializer extends JsonDeserializer<Map<String, List<String>>> {

    @Override
    public Map<String, List<String>> deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
        JsonNode node = p.getCodec().readTree(p);
        Map<String, List<String>> result = new LinkedHashMap<>();
        if (node == null || node.isNull()) {
            return result;
        }
        node.properties().forEach(entry -> {
            JsonNode value = entry.getValue();
            List<String> values = new ArrayList<>();
            if (value.isArray()) {
                value.forEach(element -> values.add(element.asText()));
            } else if (!value.isNull()) {
                values.add(value.asText());
            }
            result.put(entry.getKey(), values);
        });
        return result;
    }
}
