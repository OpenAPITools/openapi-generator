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
import org.openapitools.codegen.templating.mustache.CacheLambda.CacheContent;

import java.io.IOException;
import java.io.Writer;

/**
 * Writes rendered text that was previously cached by key.
 *
 * Syntax:
 * {{#recallScope}}key{{/recallScope}}
 */
public class RecallLambda implements Mustache.Lambda {
    private final CacheContent cacheContent;
    private final boolean clear;

    public RecallLambda(CacheContent cacheContent, boolean clear) {
        this.cacheContent = cacheContent;
        this.clear = clear;
    }

    @Override
    public void execute(Fragment fragment, Writer writer) throws IOException {
        String key = fragment.execute().trim();
        if (key.isEmpty()) {
            throw new IllegalArgumentException("recallScope key cannot be empty");
        }

        String content = this.cacheContent.contentByKey.get(key);
        if (content == null) {
            return;
        }

        if (this.clear) {
            this.cacheContent.contentByKey.remove(key);
        }

        writer.write(content);
    }
}