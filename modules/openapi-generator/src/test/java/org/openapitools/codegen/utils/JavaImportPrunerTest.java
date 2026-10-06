package org.openapitools.codegen.utils;

import org.testng.Assert;
import org.testng.annotations.Test;

import static org.openapitools.codegen.utils.JavaImportPruner.removeUnusedImports;

public class JavaImportPrunerTest {

    private static String lines(String... lines) {
        return String.join("\n", lines) + "\n";
    }

    @Test
    public void removesUnusedImports() {
        String source = lines(
                "package org.example;",
                "",
                "import java.util.List;",
                "import java.util.Locale;",
                "import java.util.Map;",
                "",
                "public class Foo {",
                "    private List<String> names;",
                "}");
        Assert.assertEquals(removeUnusedImports(source), lines(
                "package org.example;",
                "",
                "import java.util.List;",
                "",
                "public class Foo {",
                "    private List<String> names;",
                "}"));
    }

    @Test
    public void returnsSourceUnchangedWhenAllImportsAreUsed() {
        String source = lines(
                "package org.example;",
                "",
                "import java.util.List;",
                "import java.util.Map;",
                "",
                "public class Foo {",
                "    private List<Map<String, Object>> items;",
                "}");
        Assert.assertSame(removeUnusedImports(source), source);
    }

    @Test
    public void removesDuplicateImports() {
        String source = lines(
                "import java.util.Arrays;",
                "import java.util.List;",
                "import java.util.Arrays;",
                "",
                "class Foo {",
                "    List<String> names = Arrays.asList(\"a\");",
                "}");
        Assert.assertEquals(removeUnusedImports(source), lines(
                "import java.util.Arrays;",
                "import java.util.List;",
                "",
                "class Foo {",
                "    List<String> names = Arrays.asList(\"a\");",
                "}"));
    }

    @Test
    public void keepsWildcardImports() {
        String source = lines(
                "import java.util.*;",
                "import jakarta.validation.constraints.*;",
                "",
                "class Foo {",
                "}");
        Assert.assertSame(removeUnusedImports(source), source);
    }

    @Test
    public void ignoresNamesInCommentsAndLiterals() {
        String source = lines(
                "import java.util.List;",
                "import java.util.Map;",
                "import java.util.Set;",
                "",
                "class Foo {",
                "    // a List of things",
                "    /* a Map */",
                "    String set = \"Set\";",
                "    char c = '\"';",
                "    String block = \"\"\"",
                "        List Map Set",
                "        \"\"\";",
                "}");
        Assert.assertEquals(removeUnusedImports(source), lines(
                "",
                "class Foo {",
                "    // a List of things",
                "    /* a Map */",
                "    String set = \"Set\";",
                "    char c = '\"';",
                "    String block = \"\"\"",
                "        List Map Set",
                "        \"\"\";",
                "}"));
    }

    @Test
    public void keepsImportsReferencedFromJavadoc() {
        String source = lines(
                "import java.util.List;",
                "import java.util.Map;",
                "import java.io.IOException;",
                "",
                "/**",
                " * See {@link List} and {@linkplain Map#get(Object) get}.",
                " * @throws IOException never",
                " */",
                "class Foo {",
                "}");
        Assert.assertSame(removeUnusedImports(source), source);
    }

    @Test
    public void removesImportsOnlyUsedQualified() {
        String source = lines(
                "import java.util.Locale;",
                "",
                "class Foo {",
                "    String s = String.format(java.util.Locale.ROOT, \"%s\", \"x\");",
                "}");
        Assert.assertEquals(removeUnusedImports(source), lines(
                "",
                "class Foo {",
                "    String s = String.format(java.util.Locale.ROOT, \"%s\", \"x\");",
                "}"));
    }

    @Test
    public void handlesStaticImports() {
        String source = lines(
                "import static java.util.Objects.requireNonNull;",
                "import static java.util.Objects.isNull;",
                "import static java.util.Collections.*;",
                "",
                "class Foo {",
                "    Foo(Object o) { requireNonNull(o); }",
                "}");
        Assert.assertEquals(removeUnusedImports(source), lines(
                "import static java.util.Objects.requireNonNull;",
                "import static java.util.Collections.*;",
                "",
                "class Foo {",
                "    Foo(Object o) { requireNonNull(o); }",
                "}"));
    }

    @Test
    public void removesImportsClashingWithTopLevelTypes() {
        String source = lines(
                "package org.example.model;",
                "",
                "import java.util.Locale;",
                "import java.util.Objects;",
                "",
                "public class Locale {",
                "    private Locale parent;",
                "    public boolean equals(Object o) { return Objects.equals(this, o); }",
                "}");
        Assert.assertEquals(removeUnusedImports(source), lines(
                "package org.example.model;",
                "",
                "import java.util.Objects;",
                "",
                "public class Locale {",
                "    private Locale parent;",
                "    public boolean equals(Object o) { return Objects.equals(this, o); }",
                "}"));
    }

    @Test
    public void keepsImportsMatchingNestedTypeNames() {
        String source = lines(
                "import java.util.Map;",
                "",
                "class Foo {",
                "    Map<String, String> values;",
                "    static class Map {",
                "    }",
                "}");
        Assert.assertSame(removeUnusedImports(source), source);
    }

    @Test
    public void ignoresImportLikeTextInTextBlocks() {
        String source = lines(
                "import java.util.List;",
                "",
                "class Foo {",
                "    List<String> lines;",
                "    String snippet = \"\"\"",
                "import java.util.Map;",
                "\"\"\";",
                "}");
        Assert.assertSame(removeUnusedImports(source), source);
    }

    @Test
    public void collapsesBlankLinesLeftInImportSection() {
        String source = lines(
                "package org.example;",
                "",
                "import java.util.List;",
                "",
                "import java.util.Map;",
                "",
                "import java.util.Set;",
                "",
                "class Foo {",
                "    List<String> names;",
                "}");
        Assert.assertEquals(removeUnusedImports(source), lines(
                "package org.example;",
                "",
                "import java.util.List;",
                "",
                "class Foo {",
                "    List<String> names;",
                "}"));
    }

    @Test
    public void handlesSourcesWithoutImports() {
        Assert.assertNull(removeUnusedImports(null));
        String source = lines("package org.example;", "", "class Foo {", "}");
        Assert.assertSame(removeUnusedImports(source), source);
    }
}
