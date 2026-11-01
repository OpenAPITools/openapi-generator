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
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.CSharpClientCodegen;
import org.openapitools.codegen.languages.JavaCXFClientCodegen;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
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
    public void testGenericHostUsesPropertySpecificDateFormatsSymmetrically() throws IOException {
        DateTimeSchema customDateTime = new DateTimeSchema();
        customDateTime.addExtension("x-csharp-datetime-format", "yyyyMMdd'T'HHmmssK");
        DateTimeSchema nullableCustomDateTime = new DateTimeSchema();
        nullableCustomDateTime.setNullable(true);
        nullableCustomDateTime.addExtension("x-csharp-datetime-format", "yyyyMMddHHmmss");
        DateSchema customDate = new DateSchema();
        customDate.addExtension("x-csharp-date-format", "yyyyMMdd");
        OpenAPI openAPI = new OpenAPI().components(new Components().addSchemas("TemporalModel",
                new ObjectSchema()
                        .addProperty("standardDateTime", new DateTimeSchema())
                        .addProperty("customDateTime", customDateTime)
                        .addProperty("nullableCustomDateTime", nullableCustomDateTime)
                        .addProperty("customDate", customDate)));

        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        DefaultGenerator generator = new DefaultGenerator();
        ClientOptInput input = new ClientOptInput().openAPI(openAPI);
        CSharpClientCodegen codegen = new CSharpClientCodegen();
        codegen.setLibrary("generichost");
        codegen.setOutputDir(output.getAbsolutePath());
        input.config(codegen);

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getPath, Function.identity()));
        File model = getGeneratedModel(files, output, "TemporalModel");

        assertFileContains(model.toPath(),
                "public string CustomDateTimeFormat { get; private set; } = \"yyyyMMdd'T'HHmmssK\";",
                "public string NullableCustomDateTimeFormat { get; private set; } = \"yyyyMMddHHmmss\";",
                "public string CustomDateFormat { get; private set; } = \"yyyyMMdd\";",
                "standardDateTime = new Option<DateTime?>(JsonSerializer.Deserialize<DateTime>(ref utf8JsonReader, jsonSerializerOptions));",
                "customDateTime = new Option<DateTime?>(DateTimeFormatHelper.ReadDateTime(ref utf8JsonReader, CustomDateTimeFormat));",
                "nullableCustomDateTime = new Option<DateTime?>(utf8JsonReader.TokenType == JsonTokenType.Null ? (DateTime?)null : DateTimeFormatHelper.ReadDateTime(ref utf8JsonReader, NullableCustomDateTimeFormat));",
                "customDate = new Option<DateOnly?>(DateTimeFormatHelper.ReadDateOnly(ref utf8JsonReader, CustomDateFormat));",
                "writer.WritePropertyName(\"standardDateTime\");",
                "JsonSerializer.Serialize(writer, temporalModel.StandardDateTimeOption.Value!.Value, jsonSerializerOptions);",
                ".CustomDateTimeOption.Value!.Value.ToString(CustomDateTimeFormat, global::System.Globalization.CultureInfo.InvariantCulture)",
                ".NullableCustomDateTimeOption.Value!.Value.ToString(NullableCustomDateTimeFormat, global::System.Globalization.CultureInfo.InvariantCulture)",
                ".CustomDateOption.Value!.Value.ToString(CustomDateFormat, global::System.Globalization.CultureInfo.InvariantCulture)");
        assertFileNotContains(model.toPath(), "StandardDateTimeFormat");
    }

    @Test
    public void testGenericHostInjectedDateFormatOverridesProperty() throws IOException {
        OpenAPI openAPI = new OpenAPI().components(new Components().addSchemas("TemporalModel",
                new ObjectSchema().addProperty("createdAt", new DateTimeSchema())));

        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        DefaultGenerator generator = new DefaultGenerator();
        ClientOptInput input = new ClientOptInput().openAPI(openAPI);
        CSharpClientCodegen codegen = new CSharpClientCodegen();
        codegen.setLibrary("generichost");
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.useDateTimeOffset(true);
        codegen.injectModelVendorExtensions().put(
                "TemporalModel.createdAt.x-csharp-datetime-format",
                "yyyyMMddHHmmss");
        input.config(codegen);

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getPath, Function.identity()));
        File model = getGeneratedModel(files, output, "TemporalModel");

        assertFileContains(model.toPath(),
                "public string CreatedAtFormat { get; private set; } = \"yyyyMMddHHmmss\";",
                "DateTimeFormatHelper.ReadDateTimeOffset(ref utf8JsonReader, CreatedAtFormat)",
                ".CreatedAtOption.Value!.Value.ToString(CreatedAtFormat, global::System.Globalization.CultureInfo.InvariantCulture)");
    }

    @Test
    public void testGenericHostWritesUnsetAndNullInnerStringEnums() throws IOException {
        OpenAPI openAPI = new OpenAPI().components(new Components().addSchemas("Pet",
                new ObjectSchema()
                        .addProperty("status", new StringSchema()._enum(List.of("available", "sold")))
                        .addProperty("mode", new StringSchema()._enum(List.of("auto", "manual")).nullable(true))
                        .addProperty("kind", new StringSchema()._enum(List.of("cat", "dog")).nullable(true))
                        .addRequiredItem("kind")));

        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        DefaultGenerator generator = new DefaultGenerator();
        ClientOptInput input = new ClientOptInput().openAPI(openAPI);
        CSharpClientCodegen codegen = new CSharpClientCodegen();
        codegen.setLibrary("generichost");
        codegen.setOutputDir(output.getAbsolutePath());
        input.config(codegen);

        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getPath, Function.identity()));
        File model = getGeneratedModel(files, output, "Pet");

        assertFileContains(model.toPath(),
                // optional: skipped when unset, written when set
                "if (pet.StatusOption.IsSet) { var statusRawValue ="
                        + " Pet.StatusEnumToJsonValue(pet.StatusOption.Value!.Value);"
                        + " writer.WriteString(\"status\", statusRawValue); }",
                // optional and nullable: skipped when unset, null when null, written otherwise
                "if (pet.ModeOption.IsSet) {"
                        + " if (pet.ModeOption.Value == null) writer.WriteNull(\"mode\");"
                        + " else { var modeRawValue ="
                        + " Pet.ModeEnumToJsonValue(pet.ModeOption.Value!.Value);"
                        + " writer.WriteString(\"mode\", modeRawValue); } }",
                // required and nullable: null when null, written otherwise
                "if (pet.Kind == null) writer.WriteNull(\"kind\"); else {"
                        + " var kindRawValue = Pet.KindEnumToJsonValue(pet.Kind!.Value);"
                        + " writer.WriteString(\"kind\", kindRawValue); }");
    }

    @Test
    public void testGenericHostOneOfConstructorsRespectApiVisibility() throws IOException {
        Map<String, File> publicModels = generateIssue23046Models(false);
        File publicModel = publicModels.get("DynamicMetadataValue");
        assertFileContains(publicModel.toPath(),
                "public DynamicMetadataValue(string @string)",
                "public DynamicMetadataValue(decimal @decimal)",
                "public DynamicMetadataValue(bool @bool)",
                "public DynamicMetadataValue(DateTime dateTime)");
        assertFileNotContains(publicModel.toPath(), "internal DynamicMetadataValue(");
        assertFileContains(publicModels.get("ResponseOnlyValue").toPath(),
                "internal ResponseOnlyValue(string @string)",
                "internal ResponseOnlyValue(int @int)");
        assertFileNotContains(publicModels.get("ResponseOnlyValue").toPath(), "public ResponseOnlyValue(");
        assertFileContains(publicModels.get("QueryParameterValue").toPath(),
                "public QueryParameterValue(string @string)",
                "public QueryParameterValue(int @int)");
        assertFileNotContains(publicModels.get("QueryParameterValue").toPath(), "internal QueryParameterValue(");
        assertFileContains(publicModels.get("ReadOnlyValue").toPath(),
                "internal ReadOnlyValue(string @string)",
                "internal ReadOnlyValue(int @int)");
        assertFileNotContains(publicModels.get("ReadOnlyValue").toPath(), "public ReadOnlyValue(");
        assertFileContains(publicModels.get("ForbiddenValue").toPath(),
                "internal ForbiddenValue(string @string)",
                "internal ForbiddenValue(int @int)");
        assertFileNotContains(publicModels.get("ForbiddenValue").toPath(), "public ForbiddenValue(");

        File internalModel = generateIssue23046Models(true).get("DynamicMetadataValue");
        assertFileContains(internalModel.toPath(), 
                "internal DynamicMetadataValue(string @string)",
                "internal DynamicMetadataValue(decimal @decimal)",
                "internal DynamicMetadataValue(bool @bool)",
                "internal DynamicMetadataValue(DateTime dateTime)");
        assertFileNotContains(internalModel.toPath(), "public DynamicMetadataValue(");
    }

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
                        "                throw new JsonException(\"Property is required for class RequiredClass: required_nullable_enum_string.\");",
                "if (!requiredNotnullableEnumString.IsSet)\n" +
                        "                throw new JsonException(\"Property is required for class RequiredClass: required_notnullable_enum_string.\");"
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
    public void testNotEnumConstraintIsEnforced() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_1/csharp/not-enum.yaml");
        final DefaultGenerator defaultGenerator = new DefaultGenerator();
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        CSharpClientCodegen cSharpClientCodegen = new CSharpClientCodegen();
        cSharpClientCodegen.setLibrary("restsharp");
        cSharpClientCodegen.setOutputDir(output.getAbsolutePath());
        clientOptInput.config(cSharpClientCodegen);
        defaultGenerator.opts(clientOptInput);

        Map<String, File> files = defaultGenerator.generate().stream()
                .collect(Collectors.toMap(File::getPath, Function.identity()));

        Path other = Paths.get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Model", "Other.cs");
        assertNotNull(files.get(other.toString()));
        // Enforced at deserialization so oneOf matching narrows (FromJson rejects the excluded value).
        assertFileContains(other,
                "[System.Runtime.Serialization.OnDeserialized]",
                "if (this.Kind == \"known\")",
                "throw new ArgumentException(\"Invalid value for Kind, must not be a value excluded by the 'not' schema.\");");
        // Also enforced in Validate() for explicit validation.
        assertFileContains(other,
                "yield return new System.ComponentModel.DataAnnotations.ValidationResult(\"Invalid value for Kind, must not be a value excluded by the 'not' schema.\", new [] { \"Kind\" });");
        // The Known member (plain enum) must NOT get the not-enum handling.
        assertFileNotContains(Paths.get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Model", "Known.cs"),
                "must not be a value excluded by the 'not' schema");
        // A property that is ITSELF an enum (generated as a C# enum type) must not get the string-comparison
        // handling, which would not compile.
        assertFileNotContains(Paths.get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Model", "EnumAndNot.cs"),
                "must not be a value excluded by the 'not' schema", "OnDeserializedNotEnum");
        // A property named like the generated callback is renamed so it cannot collide with the method.
        assertFileContains(Paths.get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Model", "Reserved.cs"),
                "PropertyOnDeserializedNotEnum");
    }

    @Test
    public void testJsonContentHeaderUsesJsonSerialization() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_1/csharp/json-header-content.yaml");
        final DefaultGenerator defaultGenerator = new DefaultGenerator();
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        CSharpClientCodegen cSharpClientCodegen = new CSharpClientCodegen();
        cSharpClientCodegen.setLibrary("restsharp");
        cSharpClientCodegen.setOutputDir(output.getAbsolutePath());
        clientOptInput.config(cSharpClientCodegen);
        defaultGenerator.opts(clientOptInput);

        Map<String, File> files = defaultGenerator.generate().stream()
                .collect(Collectors.toMap(File::getPath, Function.identity()));

        File apiFile = files
                .get(Paths.get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Api", "DefaultApi.cs").toString());
        assertNotNull(apiFile);
        // JSON-content header is serialized as JSON, not via ParameterToString (which would emit the model's debug ToString()).
        assertFileContains(apiFile.toPath(),
                "localVarRequestOptions.HeaderParameters.Add(\"X-Json-Arg\", Org.OpenAPITools.Client.ClientUtils.ParameterToJsonString(xJsonArg)); // header parameter");
        assertFileNotContains(apiFile.toPath(),
                "localVarRequestOptions.HeaderParameters.Add(\"X-Json-Arg\", Org.OpenAPITools.Client.ClientUtils.ParameterToString(xJsonArg)); // header parameter");
        // A vendor JSON media type (application/vnd.*+json) is also serialized as JSON.
        assertFileContains(apiFile.toPath(),
                "localVarRequestOptions.HeaderParameters.Add(\"X-Vendor-Json-Arg\", Org.OpenAPITools.Client.ClientUtils.ParameterToJsonString(xVendorJsonArg)); // header parameter");
        // A regular (non-JSON) header keeps the existing behavior.
        assertFileContains(apiFile.toPath(),
                "localVarRequestOptions.HeaderParameters.Add(\"X-Plain-Arg\", Org.OpenAPITools.Client.ClientUtils.ParameterToString(xPlainArg)); // header parameter");
    }

    @Test
    public void testJsonContentHeaderUsesJsonSerializationGenericHost() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_1/csharp/json-header-content.yaml");
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

        File apiFile = files
                .get(Paths.get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Api", "DefaultApi.cs").toString());
        assertNotNull(apiFile);
        // JSON-content header is serialized with System.Text.Json using the client's serializer options.
        assertFileContains(apiFile.toPath(),
                "JsonSerializer.Serialize(xJsonArg, _jsonSerializerOptions)");
        // A vendor JSON media type (application/vnd.*+json) is also serialized as JSON.
        assertFileContains(apiFile.toPath(),
                "JsonSerializer.Serialize(xVendorJsonArg, _jsonSerializerOptions)");
        assertFileContains(apiFile.toPath(),
                "ClientUtils.ParameterToString(xPlainArg)");
    }

    @Test
    public void testUserAgentIsNotUrlEncoded() throws IOException {
        // both restsharp Configuration templates: the default one and the useIntForTimeout v7.9.0 fallback
        for (boolean useIntForTimeout : new boolean[]{false, true}) {
            File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
            output.deleteOnExit();
            final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
            final DefaultGenerator defaultGenerator = new DefaultGenerator();
            final ClientOptInput clientOptInput = new ClientOptInput();
            clientOptInput.openAPI(openAPI);
            CSharpClientCodegen cSharpClientCodegen = new CSharpClientCodegen();
            cSharpClientCodegen.setLibrary("restsharp");
            cSharpClientCodegen.setOutputDir(output.getAbsolutePath());
            cSharpClientCodegen.additionalProperties().put(CodegenConstants.HTTP_USER_AGENT, "my-client/1.2.3 (linux)");
            cSharpClientCodegen.additionalProperties().put("useIntForTimeout", useIntForTimeout);
            clientOptInput.config(cSharpClientCodegen);
            defaultGenerator.opts(clientOptInput);

            Map<String, File> files = defaultGenerator.generate().stream()
                    .collect(Collectors.toMap(File::getPath, Function.identity()));

            File configuration = files
                    .get(Paths.get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Client", "Configuration.cs").toString());
            assertNotNull(configuration);
            assertFileContains(configuration.toPath(), "UserAgent = \"my-client/1.2.3 (linux)\";");
            assertFileNotContains(configuration.toPath(), "UserAgent = WebUtility.UrlEncode(");
        }
    }

    @Test
    public void testSelectHeaderAcceptKeepsAllTypesJsonFirst() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        final DefaultGenerator defaultGenerator = new DefaultGenerator();
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        CSharpClientCodegen cSharpClientCodegen = new CSharpClientCodegen();
        cSharpClientCodegen.setLibrary("restsharp");
        cSharpClientCodegen.setOutputDir(output.getAbsolutePath());
        clientOptInput.config(cSharpClientCodegen);
        defaultGenerator.opts(clientOptInput);

        Map<String, File> files = defaultGenerator.generate().stream()
                .collect(Collectors.toMap(File::getPath, Function.identity()));

        File clientUtils = files
                .get(Paths.get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Client", "ClientUtils.cs").toString());
        assertNotNull(clientUtils);
        assertFileContains(clientUtils.toPath(), "accepts.OrderBy(accept => IsJsonMime(accept) ? 0 : 1)");
        assertFileNotContains(clientUtils.toPath(), "return \"application/json\";");
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
        openAPI.getComponents().getSchemas().get("FruitAnyOfDisc").getDiscriminator().setMapping(Map.of(
                "apple", "#/components/schemas/AppleAnyOfDisc",
                "banana", "#/components/schemas/BananaAnyOfDisc"));
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
        assertFileContains(file.toPath(),
                "ClientUtils.GetDiscriminatorValue(utf8JsonReaderDiscriminator, \"fruitType\")");

        File clientUtils = files.get(Paths
                .get(output.getAbsolutePath(), "src", "Org.OpenAPITools", "Client", "ClientUtils.cs")
                .toString());
        assertNotNull(clientUtils);
        assertFileContains(clientUtils.toPath(),
                "return GetDiscriminatorValue(utf8JsonReader, discriminator);",
                "if (utf8JsonReader.TokenType != JsonTokenType.String)",
                "The discriminator property '{discriminator}' must contain a JSON string, but found {utf8JsonReader.TokenType}.");
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
                        "                throw new JsonException(\"Property is required for class ModelWithEnumProperties: requiredInlineIntEnum.\");",
                "if (requiredInlineIntEnum.IsSet && requiredInlineIntEnum.Value == null)\n" +
                        "                throw new JsonException(\"Property is not nullable for class ModelWithEnumProperties: requiredInlineIntEnum.\");"
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

    @Test
    public void testMapResponse() throws Exception {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        final CSharpClientCodegen codegen = new CSharpClientCodegen();
        codegen.setOpenAPI(openAPI);
        Operation operation = openAPI.getPaths().get("/store/inventory").getGet();
        final CodegenOperation co = codegen.fromOperation("getInventory", "GET", operation, null);

        OperationMap operationMap = new OperationMap();
        operationMap.setOperation(co);

        OperationsMap objs = new OperationsMap();
        objs.setOperation(operationMap);
        objs.setImports(Collections.emptyList());
        codegen.postProcessOperationsWithModels(objs, Collections.emptyList());

        Assert.assertEquals(co.responses.size(), 1);
        CodegenResponse cr1 = co.responses.get(0);
        Assert.assertEquals(cr1.code, "200");
        Assert.assertEquals(cr1.baseType, "Integer");
        Assert.assertEquals(cr1.dataType, "Map<string, Integer>");
        Assert.assertFalse(cr1.isArray);
        Assert.assertFalse(cr1.isModel);
        Assert.assertTrue(cr1.isMap);
    }

    @Test
    public void testMapResponseWithModelValueIsNotFreeFormObject() throws Exception {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_1/csharp/issue_25224.yaml");
        final CSharpClientCodegen codegen = new CSharpClientCodegen();
        codegen.setOpenAPI(openAPI);
        Operation operation = openAPI.getPaths().get("/widgets").getGet();
        final CodegenOperation co = codegen.fromOperation("getWidgets", "GET", operation, null);

        Assert.assertEquals(co.responses.size(), 1);
        CodegenResponse cr = co.responses.get(0);
        Assert.assertEquals(cr.code, "200");
        Assert.assertTrue(cr.isMap);
        Assert.assertFalse(cr.isFreeFormObject);
        Assert.assertFalse(cr.isModel);
    }

    @Test
    public void testMapResponseWithModelValueGeneratesSystemDictionary() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_1/csharp/issue_25224.yaml");
        final DefaultGenerator defaultGenerator = new DefaultGenerator();
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        CSharpClientCodegen cSharpClientCodegen = new CSharpClientCodegen();
        cSharpClientCodegen.setLibrary("generichost");
        cSharpClientCodegen.setOutputDir(output.getAbsolutePath());
        cSharpClientCodegen.additionalProperties().put("packageName", "Example.Api.Sdk");
        cSharpClientCodegen.additionalProperties().put("netCoreProjectFile", true);
        clientOptInput.config(cSharpClientCodegen);
        defaultGenerator.opts(clientOptInput);

        Map<String, File> files = defaultGenerator.generate().stream()
                .collect(Collectors.toMap(File::getPath, Function.identity()));
        File api = files.get(Paths.get(output.getAbsolutePath(), "src", "Example.Api.Sdk", "Api", "DefaultApi.cs").toString());
        assertNotNull(api, "Could not find generated DefaultApi.cs");
        assertFileContains(api.toPath(), "Dictionary<string, Widget>");
        assertFileNotContains(api.toPath(), "Model.Dictionary<");
    }

    @Test(description = "conditionalSerialization: an all-optional model's public constructor must not raise the serialization flag for value-typed properties the caller never passed")
    public void testConditionalSerializationValueTypeConstructorFlags() throws IOException {
        Map<String, File> files = generateConditionalSerializationValueTypeModels(true);

        File updateCommand = files.get("UpdateThingCommand.cs");
        assertNotNull(updateCommand);
        // value-typed parameters become nullable so "not passed" is representable ...
        assertFileContains(updateCommand.toPath(),
                "public UpdateThingCommand(bool? autoRenew = default, int? period = default, int? graceDays = default, StatusEnum? status = default, RenewalModeEnum? renewalMode = default, string note = default)");
        // ... and the flag is only raised for arguments that were actually provided
        assertFileContains(updateCommand.toPath(),
                "if (autoRenew != null)\n" +
                        "        {\n" +
                        "            this._AutoRenew = autoRenew.Value;\n" +
                        "            this._flagAutoRenew = true;\n" +
                        "        }");
        // optional enums take the same guarded path
        assertFileContains(updateCommand.toPath(),
                "if (status != null)\n" +
                        "        {\n" +
                        "            this._Status = status.Value;\n" +
                        "            this._flagStatus = true;\n" +
                        "        }");
        // a nullable value type already had a meaningful null check and keeps it
        assertFileContains(updateCommand.toPath(),
                "this._GraceDays = graceDays;\n" +
                        "        if (this.GraceDays != null)\n" +
                        "        {\n" +
                        "            this._flagGraceDays = true;\n" +
                        "        }");
        // the public property surface is untouched: plain value types, flag-raising setters
        assertFileContains(updateCommand.toPath(), "public bool AutoRenew", "public int Period");
        assertFileNotContains(updateCommand.toPath(), "public bool? AutoRenew", "public int? Period");

        // required value-typed properties keep their non-nullable parameter and unconditional assignment
        File createCommand = files.get("CreateThingCommand.cs");
        assertNotNull(createCommand);
        assertFileContains(createCommand.toPath(),
                "public CreateThingCommand(string name = default, bool autoRenew = default, string note = default)");
        assertFileContains(createCommand.toPath(), "this._AutoRenew = autoRenew;");
        assertFileNotContains(createCommand.toPath(), "autoRenew.Value");
    }

    @Test(description = "without conditionalSerialization the constructor keeps plain value-typed parameters")
    public void testValueTypeConstructorUnchangedWithoutConditionalSerialization() throws IOException {
        Map<String, File> files = generateConditionalSerializationValueTypeModels(false);

        File updateCommand = files.get("UpdateThingCommand.cs");
        assertNotNull(updateCommand);
        assertFileContains(updateCommand.toPath(),
                "public UpdateThingCommand(bool autoRenew = default, int period = default, int? graceDays = default, StatusEnum? status = default, RenewalModeEnum? renewalMode = default, string note = default)");
        assertFileNotContains(updateCommand.toPath(), "_flagAutoRenew");
    }

    @Test(description = "restsharp ApiClient does not import the model namespace, so schemas named Parameter, ParameterType or Task do not collide with RestSharp/System types")
    public void testRestSharpApiClientDoesNotImportModelNamespace() throws IOException {
        Map<String, File> files = generateModelNameCollisions();

        File apiClient = files.get("ApiClient.cs");
        assertNotNull(apiClient);
        assertFileNotContains(apiClient.toPath(), "using Org.OpenAPITools.Model;");
        assertFileContains(apiClient.toPath(), "obj is Org.OpenAPITools.Model.AbstractOpenAPISchema");
    }

    @Test(description = "a schema named ValidationResult does not hide the DataAnnotations type in generated models")
    public void testValidatableModelsQualifyValidationResult() throws IOException {
        Map<String, File> files = generateModelNameCollisions();

        File model = files.get("ValidationResult.cs");
        assertNotNull(model);
        assertFileContains(model.toPath(),
                "IEnumerable<System.ComponentModel.DataAnnotations.ValidationResult> IValidatableObject.Validate(ValidationContext validationContext)",
                "yield return new System.ComponentModel.DataAnnotations.ValidationResult(");
        assertFileNotContains(model.toPath(), "IEnumerable<ValidationResult>", "new ValidationResult(");
    }

    private Map<String, File> generateModelNameCollisions() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/csharp/model-name-collisions.yaml");
        final DefaultGenerator defaultGenerator = new DefaultGenerator();
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        CSharpClientCodegen cSharpClientCodegen = new CSharpClientCodegen();
        cSharpClientCodegen.setLibrary("restsharp");
        cSharpClientCodegen.setOutputDir(output.getAbsolutePath());
        clientOptInput.config(cSharpClientCodegen);
        defaultGenerator.opts(clientOptInput);

        return defaultGenerator.generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity(), (first, second) -> first));
    }

    private Map<String, File> generateConditionalSerializationValueTypeModels(boolean conditionalSerialization) throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/csharp/conditional-serialization-value-types.yaml");
        final DefaultGenerator defaultGenerator = new DefaultGenerator();
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        CSharpClientCodegen cSharpClientCodegen = new CSharpClientCodegen();
        cSharpClientCodegen.setLibrary("httpclient");
        cSharpClientCodegen.setOutputDir(output.getAbsolutePath());
        cSharpClientCodegen.additionalProperties().put(CodegenConstants.OPTIONAL_CONDITIONAL_SERIALIZATION, String.valueOf(conditionalSerialization));
        clientOptInput.config(cSharpClientCodegen);
        defaultGenerator.opts(clientOptInput);

        return defaultGenerator.generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity(), (first, second) -> first));
    }

    private Map<String, File> generateIssue23046Models(boolean nonPublicApi) throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/csharp/issue_23046.yaml");
        final DefaultGenerator defaultGenerator = new DefaultGenerator();
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        CSharpClientCodegen cSharpClientCodegen = new CSharpClientCodegen();
        cSharpClientCodegen.setLibrary("generichost");
        cSharpClientCodegen.setOutputDir(output.getAbsolutePath());
        cSharpClientCodegen.setNonPublicApi(nonPublicApi);
        clientOptInput.config(cSharpClientCodegen);
        defaultGenerator.opts(clientOptInput);

        Map<String, File> files = defaultGenerator.generate().stream()
                .collect(Collectors.toMap(File::getPath, Function.identity()));
        Map<String, File> models = Map.of(
                "DynamicMetadataValue", getGeneratedModel(files, output, "DynamicMetadataValue"),
                "ResponseOnlyValue", getGeneratedModel(files, output, "ResponseOnlyValue"),
                "QueryParameterValue", getGeneratedModel(files, output, "QueryParameterValue"),
                "ReadOnlyValue", getGeneratedModel(files, output, "ReadOnlyValue"),
                "ForbiddenValue", getGeneratedModel(files, output, "ForbiddenValue"));
        return models;
    }

    private File getGeneratedModel(Map<String, File> files, File output, String modelName) {
        String path = Paths.get(output.getAbsolutePath(),
                "src", "Org.OpenAPITools", "Model", modelName + ".cs").toString();
        File model = files.get(path);
        assertNotNull(model, "Could not find generated model: " + path);
        return model;
    }
}
