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

        // fromJSON converts parsed JSON date strings into Date/POSIXct via helpers
        Assert.assertTrue(content.contains("as.Date(this_object$`date`)"),
                "fromJSON should convert the date property with as.Date");
        Assert.assertTrue(content.contains(".parse_datetime(this_object$`dateTime`)"),
                "fromJSON should convert the dateTime property with .parse_datetime()");

        // toSimpleType formats Date/POSIXct as ISO 8601 strings (manual formatting,
        // not jsonlite options) so the ... passthrough to toJSON remains unbroken
        Assert.assertTrue(content.contains("as.character(self$`date`)"),
                "toSimpleType should format Date fields as character for JSON serialization");
        Assert.assertTrue(content.contains(".format_datetime(self$`dateTime`)"),
                "toSimpleType should format POSIXct fields via .format_datetime()");
        Assert.assertFalse(content.contains("Date = \"ISO8601\""),
                "toJSONString should not hardcode Date = \"ISO8601\" (breaks ... passthrough)");

        // DESCRIPTION does not pin jsonlite (>= 1.0) since serialization is manual
        var description = generatedFiles.stream()
                .filter(file -> "DESCRIPTION".equals(file.getName())).findFirst();
        if (description.isEmpty()) {
            Assert.fail("`DESCRIPTION` has not been generated");
        }
        String descContent = String.join("\n", Files.readAllLines(Paths.get(description.get().getAbsolutePath())));
        Assert.assertFalse(descContent.contains("jsonlite (>= 1.0)"),
                "DESCRIPTION should not pin jsonlite (>= 1.0); serialization is handled in toSimpleType");
    }

    @Test
    public void testNullableDateAndDateTimeFields() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final DefaultGenerator defaultGenerator = new DefaultGenerator();

        RClientCodegen rClientCodegen = new RClientCodegen();
        rClientCodegen.setOutputDir(output.getAbsolutePath());

        // the spec has optional date/date-time fields plus a `nullable: true` `end` field
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/bugs/issue_24813-datetime-parsing.yaml");
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
        // fromJSON guards the nullable date conversion inside an is.null() check
        Assert.assertTrue(content.contains("if (!is.null(this_object$`end`)) {"),
                "fromJSON should guard the nullable date conversion");
        // fromJSONString uses a null-clearing conditional so a JSON null/omission
        // resets the field to NULL instead of retaining a stale value
        Assert.assertTrue(content.contains("if (is.null(this_object$`end`)) NULL else as.Date(this_object$`end`)"),
                "fromJSONString should clear NULL on omitted/null date fields (stale-value fix)");

        var dateTimeObject = generatedFiles.stream()
                .filter(file -> "date_time_object.R".equals(file.getName())).findFirst();
        if (dateTimeObject.isEmpty()) {
            Assert.fail("`date_time_object.R` has not been generated");
        }
        String dateTimeContent = String.join("\n", Files.readAllLines(Paths.get(dateTimeObject.get().getAbsolutePath())));
        // fromJSON guards the nullable date-time conversion; fromJSONString uses
        // a null-clearing conditional so a JSON null/omission resets the field
        Assert.assertTrue(dateTimeContent.contains("if (!is.null(this_object$`end`)) {"),
                "fromJSON should guard the nullable date-time conversion");
        Assert.assertTrue(dateTimeContent.contains("if (is.null(this_object$`end`)) NULL else .parse_datetime(this_object$`end`)"),
                "fromJSONString should clear NULL on omitted/null date-time fields (stale-value fix)");
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
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/bugs/issue_24813-datetime-parsing.yaml");
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

        // --- Cluster H: withZ assertion must be non-vacuous ---
        // The positive assertion above is also satisfied by withoutTimezone's identical
        // output, so assert the 'Z' was actually stripped (not retained in the literal).
        Assert.assertFalse(defaultsContent.contains("as.POSIXct(\"2020-01-01T12:00:00Z\""),
                "withZ default should have the trailing 'Z' stripped, not retained in the literal");

        // --- Cluster A: fromJSON uses .parse_datetime() helper (not inline tryFormats) ---
        var dateTimeObject = generatedFiles.stream()
                .filter(file -> "date_time_object.R".equals(file.getName())).findFirst();
        if (dateTimeObject.isEmpty()) {
            Assert.fail("`date_time_object.R` has not been generated");
        }
        String dtContent = String.join("\n", Files.readAllLines(Paths.get(dateTimeObject.get().getAbsolutePath())));
        Assert.assertTrue(dtContent.contains(".parse_datetime(this_object$`start`)"),
                "fromJSON should use .parse_datetime() for date-time fields, not inline tryFormats");
        Assert.assertFalse(dtContent.contains("tryFormats = c("),
                "model code should not contain inline tryFormats lists (replaced by .parse_datetime helper)");

        // --- Cluster B: fromJSONString clears NULL on omitted/null date-time fields ---
        Assert.assertTrue(dtContent.contains("if (is.null(this_object$`end`)) NULL else"),
                "fromJSONString should assign NULL for null/omitted date-time fields (stale-value fix)");

        // --- Cluster C: toJSONString uses stock jsonlite::toJSON (no Date/POSIXt/UTC options) ---
        Assert.assertFalse(dtContent.contains("Date = \"ISO8601\""),
                "toJSONString should not hardcode Date = \"ISO8601\" (breaks ... passthrough)");
        Assert.assertFalse(dtContent.contains("POSIXt = \"ISO8601\""),
                "toJSONString should not hardcode POSIXt = \"ISO8601\"");

        // --- Cluster D: toSimpleType formats date-time via .format_datetime() ---
        Assert.assertTrue(dtContent.contains(".format_datetime(self$`start`)"),
                "toSimpleType should format date-time fields via .format_datetime()");

        // --- Cluster G: optional date validation includes length()==1 check ---
        var dateObject = generatedFiles.stream()
                .filter(file -> "date_object.R".equals(file.getName())).findFirst();
        if (dateObject.isEmpty()) {
            Assert.fail("`date_object.R` has not been generated");
        }
        String dateContent = String.join("\n", Files.readAllLines(Paths.get(dateObject.get().getAbsolutePath())));
        Assert.assertTrue(dateContent.contains("inherits(`end`, \"Date\") && length(`end`) == 1"),
                "optional date validation should check length == 1 (scalar enforcement)");

        // --- Cluster I: fromJSONString uses baseName (not name) for date fields ---
        var fieldAlias = generatedFiles.stream()
                .filter(file -> "field_alias.R".equals(file.getName())).findFirst();
        if (fieldAlias.isEmpty()) {
            Assert.fail("`field_alias.R` has not been generated");
        }
        String aliasContent = String.join("\n", Files.readAllLines(Paths.get(fieldAlias.get().getAbsolutePath())));
        Assert.assertTrue(aliasContent.contains("this_object$`ship-date`"),
                "fromJSONString should read the JSON key (baseName `ship-date`), not the R field name (`ship_date`)");

        // --- Cluster F: api_client.R does NOT add Date/POSIXct to primitive_types ---
        var apiClient = generatedFiles.stream()
                .filter(file -> "api_client.R".equals(file.getName())).findFirst();
        if (apiClient.isEmpty()) {
            Assert.fail("`api_client.R` has not been generated");
        }
        String apiClientContent = String.join("\n", Files.readAllLines(Paths.get(apiClient.get().getAbsolutePath())));
        Assert.assertTrue(apiClientContent.contains("temporal_types <- c(\"Date\", \"POSIXct\")"),
                "api_client.R should define temporal_types separately from primitive_types");
        Assert.assertTrue(apiClientContent.contains(".parse_datetime <- function"),
                "api_client.R should define the .parse_datetime helper");
        Assert.assertTrue(apiClientContent.contains(".format_datetime <- function"),
                "api_client.R should define the .format_datetime helper");
        // The primitive_types line must NOT contain "Date" or "POSIXct"
        String primitiveLine = apiClientContent.lines()
                .filter(l -> l.contains("primitive_types <- c(")).findFirst().orElse("");
        Assert.assertFalse(primitiveLine.contains("\"Date\""),
                "primitive_types should not contain \"Date\" (model-name precedence fix)");
        Assert.assertFalse(primitiveLine.contains("\"POSIXct\""),
                "primitive_types should not contain \"POSIXct\" (model-name precedence fix)");
        Assert.assertTrue(apiClientContent.contains("temporal_types && is.character(obj)"),
                "deserializeObj model branch should guard temporal tokens with is.character(obj)");

        // --- Cluster E: API file formats date/date-time query and header params ---
        var apiFile = generatedFiles.stream()
                .filter(file -> file.getName().endsWith("_api.R")).findFirst();
        if (apiFile.isEmpty()) {
            Assert.fail("no *_api.R file has been generated");
        }
        String apiContent = String.join("\n", Files.readAllLines(Paths.get(apiFile.get().getAbsolutePath())));
        Assert.assertTrue(apiContent.contains(".format_datetime(`datetime_query`)"),
                "scalar date-time query param should be formatted via .format_datetime()");
        Assert.assertTrue(apiContent.contains(".format_datetime(`datetime_header`)"),
                "date-time header param should be formatted via .format_datetime()");
        Assert.assertTrue(apiContent.contains(".format_datetime(`datetime_array`)"),
                "array date-time query param should format items via .format_datetime()");
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
