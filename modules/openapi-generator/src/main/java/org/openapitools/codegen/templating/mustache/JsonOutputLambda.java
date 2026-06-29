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

package org.openapitools.codegen.templating.mustache;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import java.io.IOException;
import java.io.Writer;

/**
 * Writes JSON-capable template values without requiring JSON trees to be
 * materialized as strings first.
 */
public class JsonOutputLambda implements Mustache.Lambda {
    private static final ObjectMapper JSON_MAPPER = JsonMapper.builder()
            .disable(JsonGenerator.Feature.AUTO_CLOSE_TARGET)
            .build();

    private final String value;
    private final Object jsonValue;

    public JsonOutputLambda(String value) {
        this.value = value;
        this.jsonValue = null;
    }

    public JsonOutputLambda(JsonNode jsonNode) {
        this((Object) jsonNode);
    }

    public JsonOutputLambda(Object jsonValue) {
        this.value = null;
        this.jsonValue = jsonValue;
    }

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        if (jsonValue != null) {
            JSON_MAPPER.writerWithDefaultPrettyPrinter().writeValue(writer, jsonValue);
        } else if (value != null) {
            writer.write(value);
        }
    }
}
