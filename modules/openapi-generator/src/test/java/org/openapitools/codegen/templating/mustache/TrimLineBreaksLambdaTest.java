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

public class TrimLineBreaksLambdaTest {

    @Test
    public void trimsLineBreaksAsFragmentStreams() throws Exception {
        Template.Fragment fragment = mock(Template.Fragment.class);
        doAnswer(invocation -> {
            invocation.<Writer>getArgument(0).write("alpha\n\n");
            invocation.<Writer>getArgument(0).write("\n\nbeta\n");
            invocation.<Writer>getArgument(0).write("\n\ngamma");
            return null;
        }).when(fragment).execute(any(Writer.class));

        StringWriter output = new StringWriter();
        new TrimLineBreaksLambda().execute(fragment, output);

        assertEquals(output.toString(), "alpha\n\nbeta\n\ngamma");
    }

    @Test
    public void preservesSingleLineBreaks() throws Exception {
        Template.Fragment fragment = mock(Template.Fragment.class);
        doAnswer(invocation -> {
            invocation.<Writer>getArgument(0).write("alpha\nbeta");
            return null;
        }).when(fragment).execute(any(Writer.class));

        StringWriter output = new StringWriter();
        new TrimLineBreaksLambda().execute(fragment, output);

        assertEquals(output.toString(), "alpha\nbeta");
    }

    @Test
    public void trimsLineBreaksAcrossSingleCharacterWrites() throws Exception {
        Template.Fragment fragment = mock(Template.Fragment.class);
        doAnswer(invocation -> {
            invocation.<Writer>getArgument(0).write("\n");
            invocation.<Writer>getArgument(0).write("\n");
            invocation.<Writer>getArgument(0).write("\n");
            return null;
        }).when(fragment).execute(any(Writer.class));

        StringWriter output = new StringWriter();
        new TrimLineBreaksLambda().execute(fragment, output);

        assertEquals(output.toString(), "\n\n");
    }
}
