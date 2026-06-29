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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.samskivert.mustache.Template;
import org.testng.annotations.Test;

import java.io.StringWriter;

import static org.mockito.Mockito.mock;
import static org.testng.Assert.assertEquals;

public class JsonOutputLambdaTest {

    @Test
    public void writesStringValue() throws Exception {
        StringWriter output = new StringWriter();

        new JsonOutputLambda("example_value").execute(mock(Template.Fragment.class), output);

        assertEquals(output.toString(), "example_value");
    }

    @Test
    public void writesPrettyJsonNodeValue() throws Exception {
        ObjectNode node = new ObjectMapper().createObjectNode();
        node.put("name", "example");
        node.put("id", 123);
        StringWriter output = new StringWriter();

        new JsonOutputLambda(node).execute(mock(Template.Fragment.class), output);

        assertEquals(output.toString(), "{\n  \"name\" : \"example\",\n  \"id\" : 123\n}");
    }
}
