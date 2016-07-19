package io.swagger.codegen.languages;

import org.testng.Assert;
import org.testng.annotations.Test;

public class SwiftCodegenTest {

    SwiftCodegen swiftCodegen = new SwiftCodegen();

    @Test
    public void shouldNotBreakCorrectName() throws Exception {
        Assert.assertEquals(swiftCodegen.toSwiftyEnumName("EntryName"), "EntryName");
    }

    @Test
    public void testSingleWordAllCaps() throws Exception {
        Assert.assertEquals(swiftCodegen.toSwiftyEnumName("VALUE"), "Value");
    }

    @Test
    public void testSingleWordLowercase() throws Exception {
        Assert.assertEquals(swiftCodegen.toSwiftyEnumName("value"), "Value");
    }

    @Test
    public void testCapitalsWithUnderscore() throws Exception {
        Assert.assertEquals(swiftCodegen.toSwiftyEnumName("ENTRY_NAME"), "EntryName");
    }

    @Test
    public void testCapitalsWithDash() throws Exception {
        Assert.assertEquals(swiftCodegen.toSwiftyEnumName("ENTRY-NAME"), "EntryName");
    }

    @Test
    public void testCapitalsWithSpace() throws Exception {
        Assert.assertEquals(swiftCodegen.toSwiftyEnumName("ENTRY NAME"), "EntryName");
    }

    @Test
    public void testLowercaseWithUnderscore() throws Exception {
        Assert.assertEquals(swiftCodegen.toSwiftyEnumName("entry_name"), "EntryName");
    }

    @Test
    public void testDefaultPodAuthors() throws Exception {
        // Given

        // When
        swiftCodegen.processOpts();

        // Then
        final String podAuthors = (String) swiftCodegen.additionalProperties().get(SwiftCodegen.POD_AUTHORS);
        Assert.assertEquals(podAuthors, SwiftCodegen.DEFAULT_POD_AUTHORS);
    }

    @Test
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