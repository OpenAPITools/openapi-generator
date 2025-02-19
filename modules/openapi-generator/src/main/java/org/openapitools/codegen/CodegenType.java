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

package org.openapitools.codegen;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

public enum CodegenType {
    CLIENT, SERVER, DOCUMENTATION, SCHEMA, CONFIG, OTHER;

    private static Map<String, CodegenType> names = new HashMap<String, CodegenType>();

    @JsonCreator
    public static CodegenType forValue(String value) {
        return names.get(value.toLowerCase(Locale.ROOT));
    }

    @JsonValue
    public String toValue() {
        for (Map.Entry<String, CodegenType> entry : names.entrySet()) {
            if (entry.getValue() == this) {
                return entry.getKey();
            }
        }

        return null; // or fail
    }

    static {
        names.put("client", CLIENT);
        names.put("server", SERVER);
        names.put("documentation", DOCUMENTATION);
        names.put("schema", SCHEMA);
        names.put("config", CONFIG);
        names.put("other", OTHER);
    }
}
