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

/**
 * Saves template text to be used later.
 * <p>
 * Register:
 * <pre>
 * additionalProperties.put("copy", new CopyLambda(new CopyContent()));
 * </pre>
 * <p>
 * Use:
 * <pre>
 * {{#copy}}{{name}}{{/copy}}
 * </pre>
 */
public class CopyLambda implements Mustache.Lambda {
    public static class CopyContent {
        public String content;
    }

    public CopyContent copyContent;
    private final WhiteSpaceStrategy leadingWhiteSpaceStrategy;
    private final WhiteSpaceStrategy trailingWhiteSpaceStrategy;

    public enum WhiteSpaceStrategy {
        None,
        AppendLineBreakIfMissing,
        Strip,
        StripLineBreakIfPresent
    }

    public CopyLambda(CopyContent content, WhiteSpaceStrategy leadingWhiteSpaceStrategy, WhiteSpaceStrategy trailingWhiteSpaceStrategy) {
        this.copyContent = content;
        this.leadingWhiteSpaceStrategy = leadingWhiteSpaceStrategy;
        this.trailingWhiteSpaceStrategy = trailingWhiteSpaceStrategy;
    }

    @Override
    public void execute(Fragment fragment, Writer writer) throws IOException {
        String content = fragment.execute();

        if (this.leadingWhiteSpaceStrategy == WhiteSpaceStrategy.AppendLineBreakIfMissing && !content.startsWith("\n")) {
            content = "\n" + content;
        }

        if (this.leadingWhiteSpaceStrategy == WhiteSpaceStrategy.Strip) {
            content = content.stripLeading();
        }

        if (this.leadingWhiteSpaceStrategy == WhiteSpaceStrategy.StripLineBreakIfPresent && content.startsWith("\n")) {
            content = content.substring(1);
        }

        if (this.trailingWhiteSpaceStrategy == WhiteSpaceStrategy.AppendLineBreakIfMissing && !content.endsWith("\n")) {
            content = content + "\n";
        }

        if (this.trailingWhiteSpaceStrategy == WhiteSpaceStrategy.Strip) {
            content = content.stripTrailing();
        }

        if (this.trailingWhiteSpaceStrategy == WhiteSpaceStrategy.StripLineBreakIfPresent && content.endsWith("\n")) {
            content = content.substring(0, content.length() - 1);
        }

        this.copyContent.content = content;
    }
}
