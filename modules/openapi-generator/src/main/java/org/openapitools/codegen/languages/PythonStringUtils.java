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
    private static final String HEX_DIGITS = "0123456789abcdef";

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

    /**
     * Escapes {@code value} for use as the body of a triple-quoted ({@code """..."""})
     * Python string literal. Backslashes are doubled, and every '"' that is part of a
     * run of three or more, or that reaches the end of the value (where it could abut
     * the closing delimiter), is escaped so the literal cannot be terminated early.
     * Newlines and tabs are kept literal; '\r' becomes {@code \\r} and other ASCII
     * control characters (0x00-0x1f, 0x7f) become {@code \\xNN} so the generated
     * source stays readable by Python (a literal NUL would break parsing).
     */
    public static String toPythonTripleQuotedStringContent(String value) {
        StringBuilder content = new StringBuilder(value.length() + 8);
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            if (c == '\\') {
                content.append("\\\\");
            } else if (c == '\r') {
                content.append("\\r");
            } else if ((c < 0x20 && c != '\n' && c != '\t') || c == 0x7f) {
                content.append("\\x").append(HEX_DIGITS.charAt(c >> 4)).append(HEX_DIGITS.charAt(c & 0xf));
            } else {
                content.append(c);
            }
        }
        String escaped = content.toString();
        StringBuilder sb = new StringBuilder(escaped.length() + 8);
        int i = 0;
        while (i < escaped.length()) {
            int runStart = escaped.indexOf('"', i);
            if (runStart < 0) {
                sb.append(escaped, i, escaped.length());
                break;
            }
            int runEnd = runStart;
            while (runEnd < escaped.length() && escaped.charAt(runEnd) == '"') {
                runEnd++;
            }
            sb.append(escaped, i, runStart);
            if (runEnd - runStart >= 3 || runEnd == escaped.length()) {
                for (int k = runStart; k < runEnd; k++) {
                    sb.append("\\\"");
                }
            } else {
                sb.append(escaped, runStart, runEnd);
            }
            i = runEnd;
        }
        return sb.toString();
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
