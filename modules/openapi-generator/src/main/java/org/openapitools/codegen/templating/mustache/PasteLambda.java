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

import java.io.IOException;
import java.io.Writer;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template.Fragment;

/**
 * Writes text that was previously saved.
 *
 * Register:
 * <pre>
 * additionalProperties.put("paste", new PasteLambda(copyLambda, true, true, true, false));
 * </pre>
 *
 * Use:
 * <pre>
 * {{#paste}}{{/paste}}
 * </pre>
 */
public class PasteLambda implements Mustache.Lambda {
    private final CopyLambda copyLambda;
    private final Boolean stripLeading;
    private final Boolean stripTrailing;
    private final Boolean endWithLineBreak;
    private final Boolean clear;

    public PasteLambda(CopyLambda copyLambda, Boolean stripLeading, Boolean stripTrailing, Boolean endWithLineBreak, boolean clear) {
        this.copyLambda = copyLambda;
        this.stripLeading = stripLeading;
        this.stripTrailing = stripTrailing;
        this.endWithLineBreak = endWithLineBreak;
        this.clear = clear;
    }

    @Override
    public void execute(Fragment fragment, Writer writer) throws IOException {
        String content = this.copyLambda.savedContent;

        if (content == null) {
            return;
        }

        if (this.stripTrailing){
            content = content.stripTrailing();
        }
        if (this.stripLeading) {
            content = content.stripLeading();
        }
        if (this.endWithLineBreak && !content.endsWith("\n")){
            content = content + "\n";
        }
        writer.write(content);

        if (this.clear) {
            this.copyLambda.savedContent = null;
        }
    }
}
