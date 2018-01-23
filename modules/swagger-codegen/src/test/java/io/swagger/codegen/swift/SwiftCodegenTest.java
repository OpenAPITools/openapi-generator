package io.swagger.codegen.swift;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.SwiftCodegen;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.parser.OpenAPIV3Parser;
import org.testng.Assert;
import org.testng.annotations.Test;

import static io.swagger.codegen.languages.helpers.ExtensionHelper.getBooleanValue;

public class SwiftCodegenTest {

    SwiftCodegen swiftCodegen = new SwiftCodegen();

    @Test(enabled = false)
    public void shouldNotBreakCorrectName() throws Exception {
        Assert.assertEquals(swiftCodegen.toSwiftyEnumName("EntryName"), "EntryName");
    }

    @Test(enabled = false)
    public void testSingleWordAllCaps() throws Exception {
        Assert.assertEquals(swiftCodegen.toSwiftyEnumName("VALUE"), "Value");
    }

    @Test(enabled = false)
    public void testSingleWordLowercase() throws Exception {
        Assert.assertEquals(swiftCodegen.toSwiftyEnumName("value"), "Value");
    }

    @Test(enabled = false)
    public void testCapitalsWithUnderscore() throws Exception {
        Assert.assertEquals(swiftCodegen.toSwiftyEnumName("ENTRY_NAME"), "EntryName");
    }

    @Test(enabled = false)
    public void testCapitalsWithDash() throws Exception {
        Assert.assertEquals(swiftCodegen.toSwiftyEnumName("ENTRY-NAME"), "EntryName");
    }

    @Test(enabled = false)
    public void testCapitalsWithSpace() throws Exception {
        Assert.assertEquals(swiftCodegen.toSwiftyEnumName("ENTRY NAME"), "EntryName");
    }

    @Test(enabled = false)
    public void testLowercaseWithUnderscore() throws Exception {
        Assert.assertEquals(swiftCodegen.toSwiftyEnumName("entry_name"), "EntryName");
    }

    @Test(enabled = false)
    public void testSlash() throws Exception {
        Assert.assertEquals(swiftCodegen.toSwiftyEnumName("application/x-tar"), "ApplicationXTar");
    }

    @Test(description = "returns NSData when response format is binary", enabled = false)
    public void binaryDataTest() {
        // TODO: fix json file.
        final OpenAPI openAPI = new OpenAPIV3Parser().read("src/test/resources/2_0/binaryDataTest.json");
        final DefaultCodegen codegen = new SwiftCodegen();
        final String path = "/tests/binaryResponse";
        final Operation p = openAPI.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, openAPI.getComponents().getSchemas());

        Assert.assertEquals(op.returnType, "NSData");
        Assert.assertEquals(op.bodyParam.dataType, "NSData");
        Assert.assertTrue(getBooleanValue(op.bodyParam, CodegenConstants.IS_BINARY_EXT_NAME));
        Assert.assertTrue(getBooleanValue(op.responses.get(0), CodegenConstants.IS_BINARY_EXT_NAME));
    }

    @Test(description = "returns ISOFullDate when response format is date", enabled = false)
    public void dateTest() {
        // TODO: fix json file.
        final OpenAPI openAPI = new OpenAPIV3Parser().read("src/test/resources/2_0/datePropertyTest.json");
        final DefaultCodegen codegen = new SwiftCodegen();
        final String path = "/tests/dateResponse";
        final Operation p = openAPI.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, openAPI.getComponents().getSchemas());

        Assert.assertEquals(op.returnType, "ISOFullDate");
        Assert.assertEquals(op.bodyParam.dataType, "ISOFullDate");
    }

    @Test(enabled = false)
    public void testDefaultPodAuthors() throws Exception {
        // Given

        // When
        swiftCodegen.processOpts();

        // Then
        final String podAuthors = (String) swiftCodegen.additionalProperties().get(SwiftCodegen.POD_AUTHORS);
        Assert.assertEquals(podAuthors, SwiftCodegen.DEFAULT_POD_AUTHORS);
    }

    @Test(enabled = false)
    public void testPodAuthors() throws Exception {
        // Given
        final String swaggerDevs = "Swagger Devs";
        swiftCodegen.additionalProperties().put(SwiftCodegen.POD_AUTHORS, swaggerDevs);

        // When
        swiftCodegen.processOpts();

        // Then
        final String podAuthors = (String) swiftCodegen.additionalProperties().get(SwiftCodegen.POD_AUTHORS);
        Assert.assertEquals(podAuthors, swaggerDevs);
    }

}