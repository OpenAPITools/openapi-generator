package org.openapitools.codegen.templating;

import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

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
}