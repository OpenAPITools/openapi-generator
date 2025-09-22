package org.openapitools.codegen.templating;

import org.mockito.Mockito;
import org.openapitools.codegen.api.TemplatingExecutor;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.IOException;
import java.util.Map;

import static org.testng.Assert.assertEquals;

public class HandlebarsEngineAdapterTest {
    @Test(dataProvider = "handlesFileExpectations")
    public void checkHandleFiles(String input, boolean shouldHandle, String message) {
        HandlebarsEngineAdapter adapter = new HandlebarsEngineAdapter();
        boolean handles = adapter.handlesFile(input);
        assertEquals(handles, shouldHandle, message);
    }

    @DataProvider(name = "handlesFileExpectations")
    public Object[][] handlesFileExpectations() {
        // input, shouldHandle, message
        return new Object[][]{
                {"api.handlebars", true, "Expected to support handlebars extension"},
                {"api.hbs", true, "Expected to support hbs extension"},
                {"model.handlebars", true, "Expected to support handlebars extension"},
                {"model.hbs", true, "Expected to support hbs extension"},
                {"libraries/some/api.handlebars", true, "Expected to support handlebars extension for libraries"},
                {"libraries/some/api.hbs", true, "Expected to support hbs extension for libraries"},
                {"api.mustache", true, "Expected to support inferring handlebars extension from a mustache input"},
                {"model.mustache", true, "Expected to support inferring handlebars extension from a mustache input"},
                {"libraries/some/api.mustache", true, "Expected to support inferring handlebars extension from a mustache input for libraries"},
                {"libraries/some/model.mustache", true, "Expected to support inferring handlebars extension from a mustache input for libraries"},
                {".hbs", false, "Should not consider .hbs a valid file to process"},
                {".handlebars", false, "Should not consider .handlebars a valid file to process"},
                {".gitignore", false, "Should not attempt to handle .gitignore"},
                {"README.md", false, "Should not attempt to handle non-handlebars extensions (other than mustache)"}
        };
    }

    @Test(description = "verify https://github.com/jknack/handlebars.java/issues/940#issue-1111612043 is fixed")
    public void testHandlePartialTemplate() throws IOException {
        // Given
        HandlebarsEngineAdapter adapter = new HandlebarsEngineAdapter();
        TemplatingExecutor executorMock = Mockito.mock(TemplatingExecutor.class);
        Mockito.when(executorMock.getFullTemplateContents("outerTemplate.hbs")).thenReturn("Contents: {{>innerTemplate}}");
        Mockito.when(executorMock.getFullTemplateContents("innerTemplate.hbs")).thenReturn("'Specific contents'");

        // When
        String generatedFile = adapter.compileTemplate(executorMock, Map.of(), "outerTemplate.hbs");

        // Then
        assertEquals(generatedFile, "Contents: 'Specific contents'");
    }

    @Test(description = "should prioritize public getters over breaking encapsulation")
    public void testResolverPriority() throws IOException {
        // Given
        HandlebarsEngineAdapter adapter = new HandlebarsEngineAdapter();
        TemplatingExecutor executorMock = Mockito.mock(TemplatingExecutor.class);
        Mockito.when(executorMock.getFullTemplateContents("outerTemplate.hbs")).thenReturn(
                "Contents: {{#propertyObj}}\n" +
                        "  public getter: {{valueMethodAndBean}}\n" +
                        "  public method: {{valueAndMethod}}\n" +
                        "  private property: {{valueOnly}}{{/propertyObj}}");

        Map<String, Object> bundle = Map.of("propertyObj", new PropertyObject());

        // When
        String generatedFile = adapter.compileTemplate(executorMock, bundle, "outerTemplate.hbs");

        // Then
        assertEquals(generatedFile, "Contents: \n" +
                "  public getter: get_raw_data1_formatted\n" +
                "  public method: raw_data2_formatted\n" +
                "  private property: raw_data3");
    }

    static class PropertyObject {
        /**
         * getter-exposed
         */
        private final String valueMethodAndBean = "raw_data1";

        public String valueMethodAndBean() {
            return valueMethodAndBean + "_formatted";
        }

        public String getValueMethodAndBean() {
            return "get_" + valueMethodAndBean();
        }

        /**
         * method-exposed
         */
        private final String valueAndMethod = "raw_data2";

        public String valueAndMethod() {
            return valueAndMethod + "_formatted";
        }

        /**
         * private
         * note: ideally we long-term move towards respecting encapsulation where possible
         */
        @SuppressWarnings({"unused", "java:S1068"}) // this private value is still read by our HandleBars engine
        private final String valueOnly = "raw_data3";
    }
}