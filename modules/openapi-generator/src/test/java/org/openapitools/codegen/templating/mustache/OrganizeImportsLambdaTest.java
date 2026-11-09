package org.openapitools.codegen.templating.mustache;

import org.testng.annotations.Test;

import java.util.Map;

public class OrganizeImportsLambdaTest extends LambdaTest {

    @Test
    public void javaUtilWildcards() {
        // Given
        Map<String, Object> ctx = context("fixImport", new OrganizeImportsLambda(true));

        // When & Then
        test("import java.util.*\n",
                "{{#fixImport}}\n" +
                        "import java.util.*\n\n" +
                        "import java.util.List\n" +
                        "import java.util.Map\n" +
                        "{{/fixImport}}", ctx);
    }

    @Test
    public void wildCardMixed() {
        // Given
        Map<String, Object> ctx = context("fixImport", new OrganizeImportsLambda(true));

        // When & Then
        test("import jakarta.validation.*\n" +
                "import jakarta.validation.constraints.NotNull\n" +
                "import java.util.List;\n",

                "{{#fixImport}}\n" +
                "import jakarta.validation.*\n" +
                "import java.util.List;\n" +
                "import jakarta.validation.Valid\n" +
                "import jakarta.validation.constraints.NotNull\n" +
                "import java.util.List;\n" +
                "{{/fixImport}}",
                ctx);
    }

    @Test
    public void noWildCard() {
        // Given
        Map<String, Object> ctx = context("fixImport", new OrganizeImportsLambda(false));

        // When & Then
        test(
                "import jakarta.validation.*\n" +
                         "import jakarta.validation.Valid\n" +
                        "import jakarta.validation.constraints.NotNull\n" +
                        "import java.util.List;\n",

                "{{#fixImport}}\n" +
                        "import jakarta.validation.*\n" +
                        "import java.util.List;\n" +
                        "import jakarta.validation.Valid\n" +
                        "import jakarta.validation.constraints.NotNull\n" +
                        "import java.util.List;\n" +
                        "{{/fixImport}}",
                ctx);
    }

}
