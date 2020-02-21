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

package org.openapitools.codegen.cmd.utils;

import org.openapitools.codegen.utils.OptionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.testng.annotations.Test;

import java.util.Collections;
import java.util.List;

import static java.util.Arrays.asList;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;

@SuppressWarnings("static-method")
public class OptionUtilsTest {

    @Test
    public void splitCommaSeparatedList() throws Exception {
        doCommaSeparatedListTest("a,b,c", asList("a", "b", "c"));
        doCommaSeparatedListTest("a,,c", asList("a", "c"));
        doCommaSeparatedListTest("", emptyList());
        doCommaSeparatedListTest(null, emptyList());
    }

    @Test
    public void testParseCommaSeparatedTuples() throws Exception {
        doTupleListTest("a=1,b=2,c=3",
                asList(Pair.of("a", "1"), Pair.of("b", "2"), Pair.of("c", "3")));
        doTupleListTest("xyz", asList(Pair.of("xyz", "")));
        doTupleListTest("a=1,,c=3", asList(Pair.of("a", "1"), Pair.of("c", "3")));
        doTupleListTest("a=1,xyz=,c=3",
                asList(Pair.of("a", "1"), Pair.of("xyz", ""), Pair.of("c", "3")));
        doTupleListTest("a=1,xyz,c=3",
                asList(Pair.of("a", "1"), Pair.of("xyz", ""), Pair.of("c", "3")));
        doTupleListTest("a=1,=,c=3", asList(Pair.of("a", "1"), Pair.of("c", "3")));
        doTupleListTest("", emptyPairList());
        doTupleListTest(null, emptyPairList());
    }

    private static void doTupleListTest(String input, List<Pair<String, String>> expectedResults) {
        final List<Pair<String, String>> result = OptionUtils.parseCommaSeparatedTuples(input);
        assertNotNull(result);
        assertEquals(result, expectedResults);
    }

    private static void doCommaSeparatedListTest(String csvStr, List<String> expectedResults) {
        final List<String> result = OptionUtils.splitCommaSeparatedList(csvStr);
        assertNotNull(result);
        assertEquals(result, expectedResults);
    }

    private static List<Pair<String, String>> emptyPairList() {
        return Collections.emptyList();
    }

    private static List<String> emptyList() {
        return Collections.emptyList();
    }
}
