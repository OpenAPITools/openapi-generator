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

package org.openapitools.codegen.languages;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache;
import io.swagger.v3.core.util.Json;

public final class PythonStringUtils {
    private PythonStringUtils() {}

    public static String toPythonStringLiteral(String value) {
        try {
            return Json.mapper().writeValueAsString(value);
        } catch (JsonProcessingException e) {
            throw new IllegalStateException("Failed to encode Python string literal", e);
        }
    }

    public static String toPythonSingleQuotedStringLiteral(String value) {
        String doubleQuoted = toPythonStringLiteral(value);
        String contents = doubleQuoted.substring(1, doubleQuoted.length() - 1)
                .replace("\\\"", "\"")
                .replace("'", "\\'");
        return "'" + contents + "'";
    }

    public static ImmutableMap.Builder<String, Mustache.Lambda> addMustacheLambdas(
            ImmutableMap.Builder<String, Mustache.Lambda> lambdas) {
        return lambdas
                .put("pythonStringLiteral", (fragment, writer) ->
                        writer.write(toPythonStringLiteral(fragment.execute())))
                .put("pythonSingleQuotedStringLiteral", (fragment, writer) ->
                        writer.write(toPythonSingleQuotedStringLiteral(fragment.execute())));
    }
}
