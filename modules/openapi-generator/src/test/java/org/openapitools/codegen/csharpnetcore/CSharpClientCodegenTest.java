/*
 * Copyright 2020 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.csharpnetcore;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.CSharpClientCodegen;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;

public class CSharpClientCodegenTest {

    @Test
    public void testGenericHostInnerStringEnumUnknownHandlingPreservesNullBehavior() throws IOException {
        // Unknown enum values must throw for both nullable and non-nullable properties.
        // Preserve the existing null-token behavior: nullable properties record a present
        // null, while non-nullable string properties retain their existing null guard.
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec(
                "src/test/resources/3_0/csharp/petstore-with-fake-endpoints-models-for-testing-with-http-signature.yaml");
        final DefaultGenerator defaultGenerator = new DefaultGenerator();
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        CSharpClientCodegen cSharpClientCodegen = new CSharpClientCodegen();
        cSharpClientCodegen.setLibrary("generichost");
        cSharpClientCodegen.setOutputDir(output.getAbsolutePath());
        clientOptInput.config(cSharpClientCodegen);
        defaultGenerator.opts(clientOptInput);

        Map<String, File> files = defaultGenerator.generate().stream()
                .collect(Collectors.toMap(File::getPath, Function.identity()));

        File requiredClass = files.get(Paths.get(output.getAbsolutePath(),
                "src", "Org.OpenAPITools", "Model", "RequiredClass.cs").toString());
        assertNotNull(requiredClass);
        String requiredClassContents = Files.readString(requiredClass.toPath());
        assertThat(requiredClassContents)
                .containsPattern(
                        "if \\(requiredNullableEnumStringRawValue == null\\)\\s+" +
                                "requiredNullableEnumString = new Option<RequiredClass.RequiredNullableEnumStringEnum\\?>\\(null\\);")
                .containsPattern(
                        "if \\(requiredNotnullableEnumStringRawValue != null\\)\\s+\\{");
        assertThat(requiredClassContents).contains(
                "if (requiredNullableEnumStringValue == null)\n" +
                        "                                    throw new JsonException();",
                "if (requiredNotnullableEnumStringValue == null)\n" +
                        "                                    throw new JsonException();",
                "if (!requiredNullableEnumString.IsSet)\n" +
                        "                throw new ArgumentException(\"Property is required for class RequiredClass.\", nameof(requiredNullableEnumString));",
                "if (!requiredNotnullableEnumString.IsSet)\n" +
                        "                throw new ArgumentException(\"Property is required for class RequiredClass.\", nameof(requiredNotnullableEnumString));"
        );
    }

    @Test
    public void testToEnumVarName() {
        final CSharpClientCodegen codegen = new CSharpClientCodegen();
        codegen.setLibrary("restsharp");
        codegen.processOpts();

        Assert.assertEquals(codegen.toEnumVarName("FooBar", "string"), "FooBar");
        Assert.assertEquals(codegen.toEnumVarName("fooBar", "string"), "FooBar");
        Assert.assertEquals(codegen.toEnumVarName("foo-bar", "string"), "FooBar");
        Assert.assertEquals(codegen.toEnumVarName("foo_bar", "string"), "FooBar");
        Assert.assertEquals(codegen.toEnumVarName("foo bar", "string"), "FooBar");

        // The below cases do not work currently, camelize doesn't support uppercase
        // Assert.assertEquals(codegen.toEnumVarName("FOO-BAR", "string"), "FooBar");
        // Assert.assertEquals(codegen.toEnumVarName("FOO_BAR", "string"), "FooBar");
    }

    @Test
    public void testUnsigned() {
        // test unsigned integer/long
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/unsigned-test.yaml");
        CSharpClientCodegen codegen = new CSharpClientCodegen();
        codegen.setLibrary("restsharp");

        Schema test1 = openAPI.getComponents().getSchemas().get("format_test");
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();
        CodegenModel cm1 = codegen.fromModel("format_test", test1);
        Assert.assertEquals(cm1.getClassname(), "FormatTest");

        final CodegenProperty property1 = cm1.allVars.get(2);
        Assert.assertEquals(property1.baseName, "unsigned_integer");
        Assert.assertEquals(property1.dataType, "uint");
        Assert.assertEquals(property1.vendorExtensions.get("x-unsigned"), Boolean.TRUE);
        Assert.assertTrue(property1.isPrimitiveType);
        Assert.assertTrue(property1.isInteger);
        Assert.assertFalse(property1.isContainer);
        Assert.assertFalse(property1.isFreeFormObject);
        Assert.assertFalse(property1.isAnyType);

        final CodegenProperty property2 = cm1.allVars.get(4);
        Assert.assertEquals(property2.baseName, "unsigned_long");
        Assert.assertEquals(property2.dataType, "ulong");
        Assert.assertEquals(property2.vendorExtensions.get("x-unsigned"), Boolean.TRUE);
        Assert.assertTrue(property2.isPrimitiveType);
        Assert.assertTrue(property2.isLong);
        Assert.assertFalse(property2.isContainer);
        Assert.assertFalse(property2.isFreeFormObject);
        Assert.assertFalse(property2.isAnyType);
    }

    @Test
    public void testHandleConstantParams() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/java/autoset_constant.yaml");
        final DefaultGenerator defaultGenerator = new DefaultGenerator();
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        CSharpClientCodegen cSharpClientCodegen = new CSharpClientCodegen();
        cSharpClientCodegen.setLibrary("restsharp");
        cSharpClientCodegen.setOutputDir(output.getAbsolutePath());
        cSharpClientCodegen.additionalProperties().put(CodegenConstants.AUTOSET_CONSTANTS, "true");
        cSharpClientCodegen.setAutosetConstants(true);
        clientOptInput.config(cSharpClientCodegen);
        defaultGenerator.opts(clientOptInput);

        Map<String, File> files = defaultGenerator.generate().stream()
                .collect(Collectors.toMap(File::getPath, Function.identity()));

        File apiFile = files
                .get(Paths.get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Api", "HelloExampleApi.cs").toString());
        assertNotNull(apiFile);
        assertFileContains(apiFile.toPath(),
                "localVarRequestOptions.HeaderParameters.Add(\"X-CUSTOM_CONSTANT_HEADER\", Org.OpenAPITools.Client.ClientUtils.ParameterToString(\"CONSTANT_VALUE\"));");
    }

    @Test
    public void test31specAdditionalPropertiesOfOneOf() throws IOException {
        // for https://github.com/OpenAPITools/openapi-generator/pull/18772
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_1/csharp/additional_properties_oneof.yaml");
        final DefaultGenerator defaultGenerator = new DefaultGenerator();
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        CSharpClientCodegen cSharpClientCodegen = new CSharpClientCodegen();
        cSharpClientCodegen.setLibrary("restsharp");
        cSharpClientCodegen.setOutputDir(output.getAbsolutePath());
        cSharpClientCodegen.setAutosetConstants(true);
        clientOptInput.config(cSharpClientCodegen);
        defaultGenerator.opts(clientOptInput);

        Map<String, File> files = defaultGenerator.generate().stream()
                .collect(Collectors.toMap(File::getPath, Function.identity()));

        File modelFile = files
                .get(Paths.get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Model", "Response.cs").toString());
        assertNotNull(modelFile);
        assertFileContains(modelFile.toPath(),
                " Dictionary<string, ResponseResultsValue> results = default");
    }

    @Test
    public void testEnumDiscriminatorDefaultValueIsNotString() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec(
                "src/test/resources/3_0/enum_discriminator_inheritance.yaml");
        final DefaultGenerator defaultGenerator = new DefaultGenerator();
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        CSharpClientCodegen cSharpClientCodegen = new CSharpClientCodegen();
        cSharpClientCodegen.setLibrary("restsharp");
        cSharpClientCodegen.setOutputDir(output.getAbsolutePath());
        cSharpClientCodegen.setAutosetConstants(true);
        clientOptInput.config(cSharpClientCodegen);
        defaultGenerator.opts(clientOptInput);

        Map<String, File> files = defaultGenerator.generate().stream()
                .collect(Collectors.toMap(File::getPath, Function.identity()));

        Map<String, String> expectedContents = Map.of(
                "Cat", "PetTypeEnum petType = PetTypeEnum.Catty",
                "Dog", "PetTypeEnum petType = PetTypeEnum.Dog",
                "Gecko", "PetTypeEnum petType = PetTypeEnum.Gecko",
                "Chameleon", "PetTypeEnum petType = PetTypeEnum.Camo",
                "MiniVan", "CarType carType = CarType.MiniVan",
                "CargoVan", "CarType carType = CarType.CargoVan",
                "SUV", "CarType carType = CarType.SUV",
                "Truck", "CarType carType = CarType.Truck",
                "Sedan", "CarType carType = CarType.Sedan"

        );
        for (Map.Entry<String, String> e : expectedContents.entrySet()) {
            String modelName = e.getKey();
            String expectedContent = e.getValue();
            File file = files.get(Paths
                    .get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Model", modelName + ".cs")
                    .toString()
            );
            assertNotNull(file, "Could not find file for model: " + modelName);
            assertFileContains(file.toPath(), expectedContent);
        }
    }

    @Test
    public void testAnyOfDiscriminatorCreatesCompilableCode() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec(
            "src/test/resources/3_0/anyOfDiscriminatorSimple.yaml");
        final DefaultGenerator defaultGenerator = new DefaultGenerator();
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        CSharpClientCodegen cSharpClientCodegen = new CSharpClientCodegen();
        cSharpClientCodegen.setLibrary("generichost");
        cSharpClientCodegen.setOutputDir(output.getAbsolutePath());
        cSharpClientCodegen.setAutosetConstants(true);
        clientOptInput.config(cSharpClientCodegen);
        defaultGenerator.opts(clientOptInput);

        Map<String, File> files = defaultGenerator.generate().stream()
            .collect(Collectors.toMap(File::getPath, Function.identity()));

        String modelName = "FruitAnyOfDisc";
        File file = files.get(Paths
            .get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Model", modelName + ".cs")
            .toString()
        );
        assertNotNull(file, "Could not find file for model: " + modelName);
        // Should not contain this as the constructor will have two parameters instead of one
        assertFileNotContains(file.toPath(), "return new FruitAnyOfDisc(appleAnyOfDisc);");
    }

    @Test
    public void testDoubleDepthArrayAliasCSharp() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/bugs/issue_21585.yaml");
        String schemaName = "GeoJSON_MultiLineString";
        String modelName = "GeoJSONMultiLineString";
        Schema schema = ModelUtils.getSchema(openAPI, schemaName);

        CSharpClientCodegen codegen = new CSharpClientCodegen();
        codegen.setOpenAPI(openAPI);
        CodegenModel concreteModel = codegen.fromModel(modelName, schema);
        assertThat(getNames(concreteModel.vars)).isEqualTo(List.of("Type", "Coordinates", "Bbox"));
        assertThat(concreteModel.vars.get(1).getDataType()).isEqualTo("List<List<List<BigDecimal>>>");
    }

    @Test
    public void testDeepArrayAlias() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/bugs/issue_21585.yaml");
        final DefaultGenerator defaultGenerator = new DefaultGenerator();
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        CSharpClientCodegen cSharpClientCodegen = new CSharpClientCodegen();
        cSharpClientCodegen.setLibrary("httpclient");
        cSharpClientCodegen.setOutputDir(output.getAbsolutePath());
        cSharpClientCodegen.setAutosetConstants(true);
        clientOptInput.config(cSharpClientCodegen);
        defaultGenerator.opts(clientOptInput);

        Map<String, File> files = defaultGenerator.generate().stream()
                .collect(Collectors.toMap(File::getPath, Function.identity()));

        String modelName = "GeoJSONMultiLineString";
        File file = files.get(Paths
                .get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Model", modelName + ".cs")
                .toString()
        );
        assertNotNull(file, "Could not find file for model: " + modelName);
        assertFileContains(file.toPath(), "public List<List<List<decimal>>> Coordinates { get; set; }");
    }

    private List<String> getNames(List<CodegenProperty> props) {
        if (props == null) return null;
        return props.stream().map(v -> v.name).collect(Collectors.toList());
    }

    @Test
    public void testNumericEnumJsonConverterUsesNumericOperations() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/csharp/integer-enum.yaml");
        final DefaultGenerator defaultGenerator = new DefaultGenerator();
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        CSharpClientCodegen cSharpClientCodegen = new CSharpClientCodegen();
        cSharpClientCodegen.setLibrary("generichost");
        cSharpClientCodegen.setOutputDir(output.getAbsolutePath());
        clientOptInput.config(cSharpClientCodegen);
        defaultGenerator.opts(clientOptInput);

        Map<String, File> files = defaultGenerator.generate().stream()
                .collect(Collectors.toMap(File::getPath, Function.identity()));

        // Verify integer enum uses numeric JSON reader with validation
        File intEnumFile = files.get(Paths
                .get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Model", "IntegerEnum.cs")
                .toString()
        );
        assertNotNull(intEnumFile, "Could not find file for model: IntegerEnum");
        assertFileContains(intEnumFile.toPath(),
                "reader.GetInt32().ToString(System.Globalization.CultureInfo.InvariantCulture)",
                "FromStringOrDefault(rawValue)",
                "throw new JsonException()",
                "writer.WriteNumberValue(",
                "public static int ToJsonValue(IntegerEnum value)"
        );
        assertFileNotContains(intEnumFile.toPath(),
                "reader.GetString()",
                "writer.WriteStringValue(",
                ": long",
                ": byte"
        );

        File modelWithEnumPropertiesFile = files.get(Paths
                .get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Model", "ModelWithEnumProperties.cs")
                .toString()
        );
        assertNotNull(modelWithEnumPropertiesFile, "Could not find file for model: ModelWithEnumProperties");
        String modelWithEnumProperties = Files.readString(modelWithEnumPropertiesFile.toPath());

        // Unknown non-null enum values are rejected consistently for nullable and non-nullable
        // string, numeric, and byte-backed inner enums.
        assertThat(modelWithEnumProperties).contains(
                "if (inlineIntEnumValue == null)\n" +
                        "                                    throw new JsonException();",
                "if (nullableInlineIntEnumValue == null)\n" +
                        "                                    throw new JsonException();",
                "if (inlineStringEnumValue == null)\n" +
                        "                                    throw new JsonException();",
                "if (nullableInlineStringEnumValue == null)\n" +
                        "                                    throw new JsonException();",
                "if (inlineByteEnumValue == null)\n" +
                        "                                    throw new JsonException();",
                "if (nullableInlineByteEnumValue == null)\n" +
                        "                                    throw new JsonException();",
                "inlineIntEnum = new Option<ModelWithEnumProperties.InlineIntEnumEnum?>(inlineIntEnumValue);",
                "ModelWithEnumProperties.NullableInlineIntEnumEnum? nullableInlineIntEnumValue = " +
                        "ModelWithEnumProperties.NullableInlineIntEnumEnumFromStringOrDefault(nullableInlineIntEnumRawValue);",
                "nullableInlineIntEnum = new Option<ModelWithEnumProperties.NullableInlineIntEnumEnum?>(" +
                        "nullableInlineIntEnumValue);",
                "inlineStringEnum = new Option<ModelWithEnumProperties.InlineStringEnumEnum?>(inlineStringEnumValue);",
                "nullableInlineStringEnum = new Option<ModelWithEnumProperties.NullableInlineStringEnumEnum?>(" +
                        "nullableInlineStringEnumValue);"
        );

        // Numeric and byte-backed enums record an explicit JSON null as present so existing
        // nullability validation can distinguish it from a missing property.
        assertThat(modelWithEnumProperties)
                .containsPattern(
                        "if \\(utf8JsonReader.TokenType == JsonTokenType.Null\\)\\s+" +
                                "inlineIntEnum = new Option<ModelWithEnumProperties.InlineIntEnumEnum\\?>\\(null\\);")
                .containsPattern(
                        "if \\(utf8JsonReader.TokenType == JsonTokenType.Null\\)\\s+" +
                                "nullableInlineIntEnum = new Option<ModelWithEnumProperties.NullableInlineIntEnumEnum\\?>\\(null\\);")
                .containsPattern(
                        "if \\(inlineStringEnumRawValue != null\\)\\s+\\{")
                .containsPattern(
                        "if \\(nullableInlineStringEnumRawValue == null\\)\\s+" +
                                "nullableInlineStringEnum = new Option<ModelWithEnumProperties.NullableInlineStringEnumEnum\\?>\\(null\\);")
                .containsPattern(
                        "if \\(utf8JsonReader.TokenType == JsonTokenType.Null\\)\\s+" +
                                "inlineByteEnum = new Option<ModelWithEnumProperties.InlineByteEnumEnum\\?>\\(null\\);")
                .containsPattern(
                        "if \\(utf8JsonReader.TokenType == JsonTokenType.Null\\)\\s+" +
                                "nullableInlineByteEnum = new Option<ModelWithEnumProperties.NullableInlineByteEnumEnum\\?>\\(null\\);");

        // Required numeric checks distinguish a missing property from a present null.
        assertThat(modelWithEnumProperties).contains(
                "if (!requiredInlineIntEnum.IsSet)\n" +
                        "                throw new ArgumentException(\"Property is required for class ModelWithEnumProperties.\", nameof(requiredInlineIntEnum));",
                "if (requiredInlineIntEnum.IsSet && requiredInlineIntEnum.Value == null)\n" +
                        "                throw new ArgumentNullException(nameof(requiredInlineIntEnum), \"Property is not nullable for class ModelWithEnumProperties.\");"
        );

        // Verify long enum uses int64 reader with validation and actual int64 values
        File longEnumFile = files.get(Paths
                .get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Model", "LongEnum.cs")
                .toString()
        );
        assertNotNull(longEnumFile, "Could not find file for model: LongEnum");
        assertFileContains(longEnumFile.toPath(),
                "enum LongEnum : long",
                "reader.GetInt64().ToString(System.Globalization.CultureInfo.InvariantCulture)",
                "FromStringOrDefault(rawValue)",
                "throw new JsonException()",
                "writer.WriteNumberValue(",
                "public static long ToJsonValue(LongEnum value)",
                "AboveInt32Max = 2147483648",
                "Int64Max = 9223372036854775807"
        );
        assertFileNotContains(longEnumFile.toPath(),
                "reader.GetString()",
                "writer.WriteStringValue("
        );

        // Verify floating-point enums match using invariant culture and write the original numeric values
        File doubleEnumFile = files.get(Paths
                .get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Model", "DoubleEnum.cs")
                .toString()
        );
        assertNotNull(doubleEnumFile, "Could not find file for model: DoubleEnum");
        assertFileContains(doubleEnumFile.toPath(),
                "reader.GetDouble().ToString(System.Globalization.CultureInfo.InvariantCulture)",
                "(1.1d).ToString(System.Globalization.CultureInfo.InvariantCulture)",
                "writer.WriteNumberValue(",
                "public static double ToJsonValue(DoubleEnum value)",
                "return 1.1d;",
                "return -1.2d;"
        );
        assertFileNotContains(doubleEnumFile.toPath(),
                "reader.GetString()",
                "writer.WriteStringValue(",
                "return (double) value"
        );

        File floatEnumFile = files.get(Paths
                .get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Model", "FloatEnum.cs")
                .toString()
        );
        assertNotNull(floatEnumFile, "Could not find file for model: FloatEnum");
        assertFileContains(floatEnumFile.toPath(),
                "reader.GetSingle().ToString(System.Globalization.CultureInfo.InvariantCulture)",
                "(1.1f).ToString(System.Globalization.CultureInfo.InvariantCulture)",
                "public static float ToJsonValue(FloatEnum value)",
                "return 1.1f;",
                "return -1.2f;"
        );

        File decimalEnumFile = files.get(Paths
                .get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Model", "DecimalEnum.cs")
                .toString()
        );
        assertNotNull(decimalEnumFile, "Could not find file for model: DecimalEnum");
        assertFileContains(decimalEnumFile.toPath(),
                "reader.GetDecimal().ToString(System.Globalization.CultureInfo.InvariantCulture)",
                "(1.1m).ToString(System.Globalization.CultureInfo.InvariantCulture)",
                "public static decimal ToJsonValue(DecimalEnum value)",
                "return 1.1m;",
                "return -1.2m;"
        );

        File byteEnumFile = files.get(Paths
                .get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Model", "ByteEnum.cs")
                .toString()
        );
        assertNotNull(byteEnumFile, "Could not find file for model: ByteEnum");
        assertFileContains(byteEnumFile.toPath(),
                "reader.GetInt32().ToString(System.Globalization.CultureInfo.InvariantCulture)",
                "public static int ToJsonValue(ByteEnum value)",
                "writer.WriteNumberValue("
        );

        // Referenced enums use their registered converters; inline enums keep their value-mapping helpers.
        File modelFile = files.get(Paths
                .get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Model", "ModelWithEnumProperties.cs")
                .toString()
        );
        assertNotNull(modelFile, "Could not find file for model: ModelWithEnumProperties");
        assertFileContains(modelFile.toPath(),
                "JsonSerializer.Deserialize<IntegerEnum?>",
                "JsonSerializer.Deserialize<LongEnum?>",
                "JsonSerializer.Deserialize<DoubleEnum?>",
                "JsonSerializer.Deserialize<FloatEnum?>",
                "JsonSerializer.Deserialize<DecimalEnum?>",
                "JsonSerializer.Deserialize<ByteEnum?>",
                "inlineIntEnumRawValue = utf8JsonReader.GetInt32().ToString(System.Globalization.CultureInfo.InvariantCulture)",
                "inlineLongEnumRawValue = utf8JsonReader.GetInt64().ToString(System.Globalization.CultureInfo.InvariantCulture)",
                "inlineDoubleEnumRawValue = utf8JsonReader.GetDouble().ToString(System.Globalization.CultureInfo.InvariantCulture)",
                "inlineByteEnumRawValue = utf8JsonReader.GetByte().ToString(System.Globalization.CultureInfo.InvariantCulture)",
                "nullableInlineByteEnumRawValue = utf8JsonReader.GetByte().ToString(System.Globalization.CultureInfo.InvariantCulture)",
                "inlineStringEnumRawValue = utf8JsonReader.GetString()",
                "nullableInlineStringEnumRawValue = utf8JsonReader.GetString()",
                "InlineLongEnumEnum : long",
                "InlineByteEnumEnum : byte",
                "return 1.1d;",
                "return -1.2d;"
        );
    }
}
