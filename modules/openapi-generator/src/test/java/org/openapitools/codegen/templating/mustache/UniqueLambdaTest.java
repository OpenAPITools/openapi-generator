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
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;

public class UniqueLambdaTest {

    @Test
    public void uniqueValuesAreWrittenAsFragmentStreams() throws Exception {
        Template.Fragment fragment = mock(Template.Fragment.class);
        doAnswer(invocation -> {
            invocation.<Writer>getArgument(0).write("alpha\nbe");
            invocation.<Writer>getArgument(0).write("ta\nalpha\n");
            invocation.<Writer>getArgument(0).write("gamma");
            invocation.<Writer>getArgument(0).write("\nbeta");
            return null;
        }).when(fragment).execute(any(Writer.class));

        StringWriter output = new StringWriter();
        new UniqueLambda("\n", false).execute(fragment, output);

        assertEquals(output.toString(), "alpha\nbeta\ngamma");
    }

    @Test
    public void uniqueValuesPreserveTrailingEmptyValue() throws Exception {
        Template.Fragment fragment = mock(Template.Fragment.class);
        doAnswer(invocation -> {
            invocation.<Writer>getArgument(0).write("alpha\nbeta\n");
            invocation.<Writer>getArgument(0).write("beta\n");
            return null;
        }).when(fragment).execute(any(Writer.class));

        StringWriter output = new StringWriter();
        new UniqueLambda("\n", false).execute(fragment, output);

        assertEquals(output.toString(), "alpha\nbeta\n");
    }

    @Test
    public void uniqueValuesCanAppendNewLine() throws Exception {
        Template.Fragment fragment = mock(Template.Fragment.class);
        doAnswer(invocation -> {
            invocation.<Writer>getArgument(0).write("alpha\nalpha");
            return null;
        }).when(fragment).execute(any(Writer.class));

        StringWriter output = new StringWriter();
        new UniqueLambda("\n", true).execute(fragment, output);

        assertEquals(output.toString(), "alpha\n");
    }

    @Test
    public void uniqueValuesPreserveRegexDelimiterBehavior() throws Exception {
        Template.Fragment fragment = mock(Template.Fragment.class);
        when(fragment.execute()).thenReturn("alpha beta\talpha");

        StringWriter output = new StringWriter();
        new UniqueLambda("\\s+", false).execute(fragment, output);

        assertEquals(output.toString(), "alpha\\s+beta");
    }
}
