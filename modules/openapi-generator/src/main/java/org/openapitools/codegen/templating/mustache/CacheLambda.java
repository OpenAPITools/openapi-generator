/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template.Fragment;

import java.io.IOException;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;

/**
 * Caches rendered text by key.
 *
 * Syntax:
 * {{#cacheScope}}key|rendered text{{/cacheScope}}
 */
public class CacheLambda implements Mustache.Lambda {
    public static class CacheContent {
        public final Map<String, String> contentByKey = new HashMap<>();
    }

    private final CacheContent cacheContent;

    public CacheLambda(CacheContent cacheContent) {
        this.cacheContent = cacheContent;
    }

    @Override
    public void execute(Fragment fragment, Writer writer) throws IOException {
        String content = fragment.execute();

        // Accept either "key|content" or a multiline form where the first line is the key.
        int separatorIndex = content.indexOf('|');
        if (separatorIndex < 0) {
            int lineBreakIndex = findFirstLineBreakIndex(content);
            if (lineBreakIndex >= 0) {
                separatorIndex = lineBreakIndex;
            }
        }

        if (separatorIndex < 0) {
            throw new IllegalArgumentException("cacheScope requires 'key|content' or 'key\\ncontent'");
        }

        String key = content.substring(0, separatorIndex).trim();
        if (key.isEmpty()) {
            throw new IllegalArgumentException("cacheScope key cannot be empty");
        }

        int contentStartIndex = separatorIndex + 1;
        // For CRLF, skip both '\r' and '\n'. For LF-only/CR-only, skip just one character.
        if (separatorIndex < content.length() && content.charAt(separatorIndex) == '\r') {
            if (contentStartIndex < content.length() && content.charAt(contentStartIndex) == '\n') {
                contentStartIndex++;
            }
        }

        this.cacheContent.contentByKey.put(key, content.substring(contentStartIndex));
    }

    private int findFirstLineBreakIndex(String content) {
        // Keep this simple and cross-platform by checking all common line endings.
        int unix = content.indexOf('\n');
        int windows = content.indexOf('\r');

        if (unix < 0) {
            return windows;
        }

        if (windows < 0) {
            return unix;
        }

        return Math.min(unix, windows);
    }
}