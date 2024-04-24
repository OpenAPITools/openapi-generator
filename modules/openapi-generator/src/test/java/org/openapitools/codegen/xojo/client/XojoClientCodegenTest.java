package org.openapitools.codegen.xojo.client;

import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.XojoClientCodegen;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import org.testng.Assert;
import org.testng.annotations.Test;

public class XojoClientCodegenTest {

    XojoClientCodegen codegen = new XojoClientCodegen();

    @Test(enabled = true)
    public void testToEnumVarNameCapitalizedReservedWord() throws Exception {
        Assertions.assertEquals(codegen.toEnumVarName("AS", null), "Escapedas");
    }

    @Test(enabled = true)
    public void testToEnumVarNameReservedWord() throws Exception {
        Assertions.assertEquals(codegen.toEnumVarName("Public", null), "Escapedpublic");
    }

    @Test(enabled = true)
    public void testToEnumVarNameShouldNotBreakNonReservedWord() throws Exception {
        Assertions.assertEquals(codegen.toEnumVarName("Error", null), "Error");
    }

    @Test(enabled = true)
    public void testToEnumVarNameShouldNotBreakCorrectName() throws Exception {
        Assertions.assertEquals(codegen.toEnumVarName("EntryName", null), "EntryName");
    }

    @Test(enabled = true)
    public void testToEnumVarNameSingleWordAllCaps() throws Exception {
        Assertions.assertEquals(codegen.toEnumVarName("VALUE", null), "Value");
    }

    @Test(enabled = true)
    public void testToEnumVarNameSingleWordLowercase() throws Exception {
        Assertions.assertEquals(codegen.toEnumVarName("value", null), "Value");
    }

    @Test(enabled = true)
    public void testToEnumVarNameCapitalsWithUnderscore() throws Exception {
        Assertions.assertEquals(codegen.toEnumVarName("ENTRY_NAME", null), "EntryName");
    }

    @Test(enabled = true)
    public void testToEnumVarNameCapitalsWithDash() throws Exception {
        Assertions.assertEquals(codegen.toEnumVarName("ENTRY-NAME", null), "EntryName");
    }

    @Test(enabled = true)
    public void testToEnumVarNameCapitalsWithSpace() throws Exception {
        Assertions.assertEquals(codegen.toEnumVarName("ENTRY NAME", null), "EntryName");
    }

    @Test(enabled = true)
    public void testToEnumVarNameLowercaseWithUnderscore() throws Exception {
        Assertions.assertEquals(codegen.toEnumVarName("entry_name", null), "EntryName");
    }

    @Test(enabled = true)
    public void testToEnumVarNameStartingWithNumber() throws Exception {
        Assertions.assertEquals(codegen.toEnumVarName("123EntryName", null), "Escaped123EntryName");
        Assertions.assertEquals(codegen.toEnumVarName("123Entry_name", null), "Escaped123EntryName");
        Assertions.assertEquals(codegen.toEnumVarName("123EntryName123", null), "Escaped123EntryName123");
    }

    @Test(enabled = true)
    public void testToEnumVarNameSpecialCharacters() throws Exception {
        Assertions.assertEquals(codegen.toEnumVarName("1:1", null), "Escaped1Colon1");
        Assertions.assertEquals(codegen.toEnumVarName("1:One", null), "Escaped1ColonOne");
        Assertions.assertEquals(codegen.toEnumVarName("Apple&Pie", null), "AppleAmpersandPie");
        Assertions.assertEquals(codegen.toEnumVarName("$", null), "Dollar");
        Assertions.assertEquals(codegen.toEnumVarName("+1", null), "EscapedPlus1");
        Assertions.assertEquals(codegen.toEnumVarName(">=", null), "GreaterThanOrEqualTo");
    }

    @Test(description = "returns Data when response format is binary", enabled = true)
    public void binaryDataTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/binaryDataTest.json");
        final DefaultCodegen codegen = new XojoClientCodegen();
        codegen.setOpenAPI(openAPI);
        final String path = "/tests/binaryResponse";
        final Operation p = openAPI.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, null);

        Assertions.assertEquals(op.returnType, "FolderItem");
        Assertions.assertEquals(op.bodyParam.dataType, "FolderItem");
        Assertions.assertTrue(op.bodyParam.isBinary);
        Assertions.assertTrue(op.responses.get(0).isBinary);
    }

    @Test(description = "returns Date when response format is date per default", enabled = true)
    public void dateDefaultTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/datePropertyTest.json");
        final DefaultCodegen codegen = new XojoClientCodegen();
        codegen.setOpenAPI(openAPI);
        final String path = "/tests/dateResponse";
        final Operation p = openAPI.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, null);

        Assertions.assertEquals(op.returnType, "Date");
        Assertions.assertEquals(op.bodyParam.dataType, "Date");
    }

    @Test(description = "type from languageSpecificPrimitives should not be prefixed", enabled = true)
    public void prefixExceptionTest() {
        final DefaultCodegen codegen = new XojoClientCodegen();
        codegen.setModelNamePrefix("API");

        final String result = codegen.toModelName("Currency");
        Assertions.assertEquals(result, "Currency");
    }

    @Test(description = "type from languageSpecificPrimitives should not be suffixed", enabled = true)
    public void suffixExceptionTest() {
        final DefaultCodegen codegen = new XojoClientCodegen();
        codegen.setModelNameSuffix("API");

        final String result = codegen.toModelName("Currency");
        Assertions.assertEquals(result, "Currency");
    }

    @Test(description = "Other types should be prefixed", enabled = true)
    public void prefixTest() {
        final DefaultCodegen codegen = new XojoClientCodegen();
        codegen.setModelNamePrefix("API");

        final String result = codegen.toModelName("MyType");
        Assertions.assertEquals(result, "APIMyType");
    }

    @Test(description = "Other types should be suffixed", enabled = true)
    public void suffixTest() {
        final DefaultCodegen codegen = new XojoClientCodegen();
        codegen.setModelNameSuffix("API");

        final String result = codegen.toModelName("MyType");
        Assertions.assertEquals(result, "MyTypeAPI");
    }
}
