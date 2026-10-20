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

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import java.io.IOException;
import java.io.Writer;

/**
 * Prefixes rendered fragment lines without applying additional indentation.
 */
public class PrefixLambda implements Mustache.Lambda {
    private final String prefix;
    private final boolean prefixFirstLine;

    public PrefixLambda(String prefix, boolean prefixFirstLine) {
        this.prefix = prefix;
        this.prefixFirstLine = prefixFirstLine;
    }

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        String text = fragment.execute();
        if (text == null || text.isEmpty()) {
            return;
        }

        String[] lines = text.split("\n", -1);
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < lines.length; i++) {
            if (prefixFirstLine || i > 0) {
                sb.append(prefix);
            }
            sb.append(lines[i]);
            if (i < lines.length - 1) {
                sb.append("\n");
            }
        }

        writer.write(sb.toString());
    }
}

