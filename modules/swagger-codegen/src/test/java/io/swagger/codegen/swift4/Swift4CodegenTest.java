package io.swagger.codegen.swift4;

import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.Swift4Codegen;
import io.swagger.models.Operation;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;
import org.testng.Assert;
import org.testng.annotations.Test;

public class Swift4CodegenTest {

    Swift4Codegen swiftCodegen = new Swift4Codegen();

    @Test
    public void testCapitalizedReservedWord() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("AS", null), "_as");
    }

    @Test
    public void testReservedWord() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("Public", null), "_public");
    }

    @Test
    public void shouldNotBreakNonReservedWord() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("Error", null), "error");
    }

    @Test
    public void shouldNotBreakCorrectName() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("EntryName", null), "entryName");
    }

    @Test
    public void testSingleWordAllCaps() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("VALUE", null), "value");
    }

    @Test
    public void testSingleWordLowercase() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("value", null), "value");
    }

    @Test
    public void testCapitalsWithUnderscore() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("ENTRY_NAME", null), "entryName");
    }

    @Test
    public void testCapitalsWithDash() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("ENTRY-NAME", null), "entryName");
    }

    @Test
    public void testCapitalsWithSpace() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("ENTRY NAME", null), "entryName");
    }

    @Test
    public void testLowercaseWithUnderscore() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("entry_name", null), "entryName");
    }

    @Test
    public void testStartingWithNumber() throws Exception {
        Assert.assertEquals(swiftCodegen.toEnumVarName("123EntryName", null), "_123entryName");
        Assert.assertEquals(swiftCodegen.toEnumVarName("123Entry_name", null), "_123entryName");
        Assert.assertEquals(swiftCodegen.toEnumVarName("123EntryName123", null), "_123entryName123");
    }

    @Test(description = "returns Data when response format is binary")
    public void binaryDataTest() {
        final Swagger model = new SwaggerParser().read("src/test/resources/2_0/binaryDataTest.json");
        final DefaultCodegen codegen = new Swift4Codegen();
        final String path = "/tests/binaryResponse";
        final Operation p = model.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, model.getDefinitions());

        Assert.assertEquals(op.returnType, "Data");
        Assert.assertEquals(op.bodyParam.dataType, "Data");
        Assert.assertTrue(op.bodyParam.isBinary);
        Assert.assertTrue(op.responses.get(0).isBinary);
    }

    @Test(description = "returns Date when response format is date")
    public void dateTest() {
        final Swagger model = new SwaggerParser().read("src/test/resources/2_0/datePropertyTest.json");
        final DefaultCodegen codegen = new Swift4Codegen();
        final String path = "/tests/dateResponse";
        final Operation p = model.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, model.getDefinitions());

        Assert.assertEquals(op.returnType, "Date");
        Assert.assertEquals(op.bodyParam.dataType, "Date");
    }

    @Test
    public void testDefaultPodAuthors() throws Exception {
        // Given

        // When
        swiftCodegen.processOpts();

        // Then
        final String podAuthors = (String) swiftCodegen.additionalProperties().get(Swift4Codegen.POD_AUTHORS);
        Assert.assertEquals(podAuthors, Swift4Codegen.DEFAULT_POD_AUTHORS);
    }

    @Test
    public void testPodAuthors() throws Exception {
        // Given
        final String swaggerDevs = "Swagger Devs";
        swiftCodegen.additionalProperties().put(Swift4Codegen.POD_AUTHORS, swaggerDevs);

        // When
        swiftCodegen.processOpts();

        // Then
        final String podAuthors = (String) swiftCodegen.additionalProperties().get(Swift4Codegen.POD_AUTHORS);
        Assert.assertEquals(podAuthors, swaggerDevs);
    }

}
