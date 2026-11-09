package org.openapitools.codegen.templating.mustache;

import org.testng.annotations.Test;

import java.util.Map;

public class OrganizeImportsLambdaTest extends LambdaTest {

    @Test
    public void organizeImportTest() {
        // Given
        Map<String, Object> ctx = context("fixImport", new OrganizeImportsLambda());

        // When & Then
        test(
                "import jakarta.validation.constraints.NotNull\n" +
                "import java.util.*\n"+
                "import java.util.List\n" +
                "import java.util.Map\n",
                "{{#fixImport}}\n" +
                        "import java.util.List\n" +
                        "import java.util.*\n\n" +
                        "import java.util.Map\n" +
                        "import jakarta.validation.constraints.NotNull\n" +
                        "{{/fixImport}}", ctx);
    }
}
