/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

package org.openapitools.codegen.r;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.RClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.regex.Pattern;

public class RClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final RClientCodegen codegen = new RClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final RClientCodegen codegen = new RClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final RClientCodegen codegen = new RClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testTypeMappingDateAndDateTime() throws Exception {
        final RClientCodegen codegen = new RClientCodegen();
        codegen.processOpts();

        // `date` maps to the R Date class, `date-time` to POSIXct (the concrete POSIXt subclass)
        Assert.assertEquals(codegen.getTypeDeclaration(new StringSchema().format("date")), "Date");
        Assert.assertEquals(codegen.getTypeDeclaration(new StringSchema().format("date-time")), "POSIXct");
        // both are language-specific primitives (no import, primitive template branches)
        Assert.assertTrue(codegen.languageSpecificPrimitives().contains("Date"));
        Assert.assertTrue(codegen.languageSpecificPrimitives().contains("POSIXct"));
    }

    @Test
    public void testDateAndDateTimeModelGeneration() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final DefaultGenerator defaultGenerator = new DefaultGenerator();

        RClientCodegen rClientCodegen = new RClientCodegen();
        rClientCodegen.setOutputDir(output.getAbsolutePath());

        // the test spec's `FormatTest` model carries `date` (default 2019-07-19) and
        // `dateTime` (default 2015-10-28T14:38:02Z) properties
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/r/petstore.yaml");
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        clientOptInput.config(rClientCodegen);
        defaultGenerator.opts(clientOptInput);

        List<File> generatedFiles = defaultGenerator.generate();
        var formatTestModel = generatedFiles.stream()
                .filter(file -> "format_test.R".equals(file.getName())).findFirst();
        if (formatTestModel.isEmpty()) {
            Assert.fail("`format_test.R` has not been generated");
        }
        String content = String.join("\n", Files.readAllLines(Paths.get(formatTestModel.get().getAbsolutePath())));

        // optional property defaults construct real R temporal objects
        // (the `date` property is required, so its default is not rendered in the initialize signature)
        Assert.assertTrue(content.contains("as.POSIXct(\"2015-10-28T14:38:02\", format = \"%Y-%m-%dT%H:%M:%OS\", tz = \"UTC\")"),
                "dateTime property default should be emitted as as.POSIXct(..., tz = \"UTC\")");

        // initialize() validation accepts the real classes (POSIXt covers POSIXct and POSIXlt)
        Assert.assertTrue(content.contains("inherits(`date`, \"Date\")"),
                "date property validation should check inherits(x, \"Date\")");
        Assert.assertTrue(content.contains("inherits(`dateTime`, \"POSIXt\")"),
                "dateTime property validation should check inherits(x, \"POSIXt\")");

        // fromJSON converts parsed JSON date strings into Date/POSIXct
        Assert.assertTrue(content.contains("as.Date(this_object$`date`)"),
                "fromJSON should convert the date property with as.Date");
        Assert.assertTrue(content.contains("as.POSIXct(this_object$`dateTime`"),
                "fromJSON should convert the dateTime property with as.POSIXct");

        // toSimpleType leaves Date/POSIXct as native R objects (no manual formatting);
        // jsonlite::toJSON handles ISO 8601 serialization via Date/POSIXt/UTC options
        Assert.assertFalse(content.contains("as.character(self$`date`)"),
                "toSimpleType should not manually format Date fields; jsonlite handles serialization");
        Assert.assertFalse(content.contains("format(self$`dateTime`"),
                "toSimpleType should not manually format POSIXct fields; jsonlite handles serialization");
        Assert.assertTrue(content.contains("Date = \"ISO8601\", POSIXt = \"ISO8601\", UTC = TRUE"),
                "toJSONString should pass ISO 8601 options to jsonlite::toJSON");

        // DESCRIPTION pins jsonlite (>= 1.0) for the ISO 8601 serialization options
        var description = generatedFiles.stream()
                .filter(file -> "DESCRIPTION".equals(file.getName())).findFirst();
        if (description.isEmpty()) {
            Assert.fail("`DESCRIPTION` has not been generated");
        }
        String descContent = String.join("\n", Files.readAllLines(Paths.get(description.get().getAbsolutePath())));
        Assert.assertTrue(descContent.contains("jsonlite (>= 1.0)"),
                "DESCRIPTION should pin jsonlite (>= 1.0) for ISO 8601 serialization support");
    }

    @Test
    public void testNullableDateAndDateTimeFields() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final DefaultGenerator defaultGenerator = new DefaultGenerator();

        RClientCodegen rClientCodegen = new RClientCodegen();
        rClientCodegen.setOutputDir(output.getAbsolutePath());

        // the spec has optional date/date-time fields plus a `nullable: true` `end` field
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/r/rproblems.yaml");
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        clientOptInput.config(rClientCodegen);
        defaultGenerator.opts(clientOptInput);

        // nullable temporal fields must stay optional: constructor defaults are NULL and
        // validation is guarded, so omitting the field (or passing NULL) never trips
        // the inherits(x, "Date"/"POSIXt") check.
        // NOTE: generate() must be called exactly once; processOpts() is not idempotent
        // (re-running it NPEs on the errorObjectType key it stores with a null value).
        List<File> generatedFiles = defaultGenerator.generate();

        var dateObject = generatedFiles.stream()
                .filter(file -> "date_object.R".equals(file.getName())).findFirst();
        if (dateObject.isEmpty()) {
            Assert.fail("`date_object.R` has not been generated");
        }
        String content = String.join("\n", Files.readAllLines(Paths.get(dateObject.get().getAbsolutePath())));
        Assert.assertTrue(content.contains("initialize = function(`start` = NULL, `end` = NULL"),
                "nullable date fields should default to NULL in the constructor signature");
        Assert.assertTrue(content.contains("if (!is.null(`end`)) {"),
                "date field validation should be guarded so NULL is accepted");
        // a JSON null must keep the field NULL (not coerce it to a zero-length Date),
        // in both fromJSON and fromJSONString
        Assert.assertEquals(content.split(Pattern.quote("if (!is.null(this_object$`end`)) {"), -1).length - 1, 2,
                "fromJSON and fromJSONString must both guard the nullable date conversion");

        var dateTimeObject = generatedFiles.stream()
                .filter(file -> "date_time_object.R".equals(file.getName())).findFirst();
        if (dateTimeObject.isEmpty()) {
            Assert.fail("`date_time_object.R` has not been generated");
        }
        String dateTimeContent = String.join("\n", Files.readAllLines(Paths.get(dateTimeObject.get().getAbsolutePath())));
        Assert.assertEquals(dateTimeContent.split(Pattern.quote("if (!is.null(this_object$`end`)) {"), -1).length - 1, 2,
                "fromJSON and fromJSONString must both guard the nullable date-time conversion");
    }

    @Test
    public void testDateArrayAndDateTimeDefaults() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final DefaultGenerator defaultGenerator = new DefaultGenerator();

        RClientCodegen rClientCodegen = new RClientCodegen();
        rClientCodegen.setOutputDir(output.getAbsolutePath());

        // the spec carries a DateArrayObject (required arrays of date / date-time) and a
        // DateTimeDefaults model whose optional date-time fields carry zone-aware and
        // zone-less defaults that exercise rDateTime() UTC normalization
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/r/rproblems.yaml");
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        clientOptInput.config(rClientCodegen);
        defaultGenerator.opts(clientOptInput);

        // NOTE: generate() must be called exactly once; processOpts() is not idempotent
        // (re-running it NPEs on the errorObjectType key it stores with a null value).
        List<File> generatedFiles = defaultGenerator.generate();

        // --- DateArrayObject: container item validation uses inherits(x, "Date"/"POSIXt") ---
        var dateArrayObject = generatedFiles.stream()
                .filter(file -> "date_array_object.R".equals(file.getName())).findFirst();
        if (dateArrayObject.isEmpty()) {
            Assert.fail("`date_array_object.R` has not been generated");
        }
        String arrayContent = String.join("\n", Files.readAllLines(Paths.get(dateArrayObject.get().getAbsolutePath())));
        // the sapply() validation for the `dates` array must check inherits(x, "Date")
        Assert.assertTrue(arrayContent.contains("sapply(`dates`, function(x) stopifnot(inherits(x, \"Date\")))"),
                "array-of-date validation should check inherits(x, \"Date\") per item");
        // the sapply() validation for the `dateTimes` array must check inherits(x, "POSIXt")
        Assert.assertTrue(arrayContent.contains("sapply(`dateTimes`, function(x) stopifnot(inherits(x, \"POSIXt\")))"),
                "array-of-date-time validation should check inherits(x, \"POSIXt\") per item");

        // --- DateTimeDefaults: rDateTime() normalizes every default to UTC ---
        var dateTimeDefaults = generatedFiles.stream()
                .filter(file -> "date_time_defaults.R".equals(file.getName())).findFirst();
        if (dateTimeDefaults.isEmpty()) {
            Assert.fail("`date_time_defaults.R` has not been generated");
        }
        String defaultsContent = String.join("\n", Files.readAllLines(Paths.get(dateTimeDefaults.get().getAbsolutePath())));
        // withZ: the trailing 'Z' is stripped; the instant is unchanged (12:00:00 UTC)
        Assert.assertTrue(defaultsContent.contains("as.POSIXct(\"2020-01-01T12:00:00\", format = \"%Y-%m-%dT%H:%M:%OS\", tz = \"UTC\")"),
                "withZ default should be emitted with the trailing 'Z' stripped (same UTC instant)");
        // withOffset: +05:00 is shifted to UTC (12:00:00+05:00 == 07:00:00 UTC)
        Assert.assertTrue(defaultsContent.contains("as.POSIXct(\"2020-01-01T07:00:00\", format = \"%Y-%m-%dT%H:%M:%OS\", tz = \"UTC\")"),
                "withOffset default should be normalized from +05:00 to UTC (07:00:00)");
        // withoutTimezone: a zone-less value is interpreted as UTC (12:00:00)
        Assert.assertTrue(defaultsContent.contains("as.POSIXct(\"2020-01-01T12:00:00\", format = \"%Y-%m-%dT%H:%M:%OS\", tz = \"UTC\")"),
                "withoutTimezone default should be interpreted as UTC (12:00:00)");
    }

    @Test
    public void testNullCheckOnEnumValues() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final DefaultGenerator defaultGenerator = new DefaultGenerator();

        RClientCodegen rClientCodegen = new RClientCodegen();
        rClientCodegen.setOutputDir(output.getAbsolutePath());

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/bugs/issue_18016.yaml");
        final ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openAPI);
        clientOptInput.config(rClientCodegen);
        defaultGenerator.opts(clientOptInput);

        var petsApi = defaultGenerator.generate().stream()
                .filter(file -> "pets_api.R".equals(file.getName())).findFirst();
        if (petsApi.isEmpty()) {
            Assert.fail("`pets_api.R` have not been generated");
        }
        var isIfCondition = Pattern.compile("^\\s*(?!<#)\\s*if.*\\s%in%\\s.*").asPredicate();
        var containsNullCheck = Pattern.compile("![(\\s]*is\\.null").asPredicate();
        var hit = false;
        for (var line : Files.readAllLines(Paths.get(petsApi.get().getAbsolutePath()))) {
            if (isIfCondition.test(line)) {
                hit = true;
                Assert.assertTrue(containsNullCheck.test(line), "Null check is missing in line: " + line);
            }
        }
        Assert.assertTrue(hit, "No if statement for enum found");
    }
}
