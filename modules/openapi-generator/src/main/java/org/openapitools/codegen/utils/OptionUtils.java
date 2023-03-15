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

package org.openapitools.codegen.utils;

import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.List;

import static org.apache.commons.lang3.StringUtils.isNotEmpty;

public class OptionUtils {

    public static List<Pair<String, String>> parseCommaSeparatedTuples(final String input) {

        final List<Pair<String, String>> results = new ArrayList<Pair<String, String>>();

        final List<String> tuples = splitCommaSeparatedList(input);

        for (String tuple : tuples) {
            int ix = tuple.indexOf('=');
            if (ix > 0 && ix <= tuple.length() - 1) {
                final Pair<String, String> pair = Pair.of(tuple.substring(0, ix), tuple.substring(ix + 1));
                results.add(pair);
            } else if (ix < 0){
                final Pair<String, String> pair = Pair.of(tuple, "");
                results.add(pair);
            }
        }

        return results;
    }

    public static List<String> splitCommaSeparatedList(String input) {

        List<String> results = new ArrayList<String>();

        if(input != null && !input.isEmpty()) {
            for (String value : input.split(",")) {
                if(isNotEmpty(value))
                results.add(value);
            }
        }

        return results;
    }
}
