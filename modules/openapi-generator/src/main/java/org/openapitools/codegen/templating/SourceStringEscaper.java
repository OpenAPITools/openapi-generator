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

package org.openapitools.codegen.templating;

/**
 * Encoders for values inserted into generated source. These methods accept
 * parsed, unescaped data and return either literal contents or a complete
 * source literal.
 */
public final class SourceStringEscaper {
    private SourceStringEscaper() {
    }

    public static String javaStringLiteral(String input) {
        return "\"" + javaStringContent(input) + "\"";
    }

    public static String javaStringContent(String input) {
        return escape(input, false);
    }

    public static String kotlinStringLiteral(String input) {
        return "\"" + kotlinStringContent(input) + "\"";
    }

    public static String kotlinStringContent(String input) {
        return escape(input, true);
    }

    /**
     * Protect a value rendered inside a Java/Kotlin block comment while
     * retaining its literal displayed meaning in standard documentation renderers.
     */
    public static String docText(String input) {
        if (input == null) {
            return "";
        }
        return input
                .replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\"", "&quot;")
                .replace("'", "&#39;")
                // Java processes Unicode escapes before recognizing comments.
                .replace("\\", "&#92;")
                .replace("/*", "&#47;*")
                .replace("*/", "*&#47;")
                .replace("\r\n", "\n")
                .replace("\r", "\n")
                .replace("\n", "\n * ");
    }

    private static String escape(String input, boolean kotlin) {
        if (input == null) {
            return "";
        }

        StringBuilder result = new StringBuilder(input.length() + 16);
        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            switch (c) {
                case '\\':
                    result.append("\\\\");
                    break;
                case '"':
                    result.append("\\\"");
                    break;
                case '$':
                    if (kotlin) {
                        result.append("\\$");
                    } else {
                        result.append(c);
                    }
                    break;
                case '\b':
                    result.append("\\b");
                    break;
                case '\t':
                    result.append("\\t");
                    break;
                case '\n':
                    result.append("\\n");
                    break;
                case '\f':
                    result.append(kotlin ? "\\u000c" : "\\f");
                    break;
                case '\r':
                    result.append("\\r");
                    break;
                default:
                    if (c < 0x20) {
                        if (kotlin) {
                            result.append(String.format("\\u%04x", (int) c));
                        } else {
                            result.append('\\');
                            result.append((char) ('0' + ((c >> 6) & 7)));
                            result.append((char) ('0' + ((c >> 3) & 7)));
                            result.append((char) ('0' + (c & 7)));
                        }
                    } else {
                        result.append(c);
                    }
                    break;
            }
        }
        return result.toString();
    }
}
