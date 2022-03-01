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

package org.openapitools.codegen.templating.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import java.io.IOException;
import java.io.Writer;
import java.util.HashMap;

public class StringReplaceLambda implements Mustache.Lambda {
    private HashMap<String, String> replaceMap;

    /**
     * Constructs a new instance of {@link StringReplaceLambda}, with an indent count of 4 spaces
     */
    public StringReplaceLambda(HashMap<String, String> replaceMap) {
        this.replaceMap = replaceMap;
    }

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        String text = fragment.execute();
        if (text == null || text.length() == 0) {
            return;
        }

        String[] lines = text.split(System.lineSeparator());
        String line = "";
        for (String s : lines) {
            line = s;

            for (String key : replaceMap.keySet()) {
                line = line.replace(key, replaceMap.get(key));
            }
        }
        writer.write(line);
    }
}
