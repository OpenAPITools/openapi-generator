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
import org.openapitools.codegen.templating.mustache.CopyLambda.CopyContent;

import java.io.IOException;
import java.io.Writer;

/**
 * Writes text that was previously saved.
 * <p>
 * Register:
 * <pre>
 * additionalProperties.put("paste", new PasteLambda(copyContent, false));
 * </pre>
 * <p>
 * Use:
 * <pre>
 * {{#paste}}{{/paste}}
 * </pre>
 */
public class PasteLambda implements Mustache.Lambda {
    private final CopyContent copyContent;
    private final Boolean clear;

    public PasteLambda(CopyContent copyContent, boolean clear) {
        this.copyContent = copyContent;
        this.clear = clear;
    }

    @Override
    public void execute(Fragment fragment, Writer writer) throws IOException {
        String content = this.copyContent.content;

        if (content == null) {
            return;
        }

        if (this.clear) {
            this.copyContent.content = null;
        }

        writer.write(content);
    }
}
