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

import com.samskivert.mustache.Template;
import org.testng.annotations.Test;

import java.io.StringWriter;
import java.io.Writer;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.testng.Assert.assertEquals;

public class RemoveDoubleQuoteLambdaTest {

    @Test
    public void removesDoubleQuotesAsFragmentStreams() throws Exception {
        Template.Fragment fragment = mock(Template.Fragment.class);
        doAnswer(invocation -> {
            invocation.<Writer>getArgument(0).write("\"alpha");
            invocation.<Writer>getArgument(0).write("\"be\"ta");
            invocation.<Writer>getArgument(0).write("gamma\"");
            return null;
        }).when(fragment).execute(any(Writer.class));

        StringWriter output = new StringWriter();
        new RemoveDoubleQuoteLambda().execute(fragment, output);

        assertEquals(output.toString(), "alphabetagamma");
    }

    @Test
    public void preservesTextWithoutDoubleQuotes() throws Exception {
        Template.Fragment fragment = mock(Template.Fragment.class);
        doAnswer(invocation -> {
            invocation.<Writer>getArgument(0).write("alpha beta");
            return null;
        }).when(fragment).execute(any(Writer.class));

        StringWriter output = new StringWriter();
        new RemoveDoubleQuoteLambda().execute(fragment, output);

        assertEquals(output.toString(), "alpha beta");
    }
}
