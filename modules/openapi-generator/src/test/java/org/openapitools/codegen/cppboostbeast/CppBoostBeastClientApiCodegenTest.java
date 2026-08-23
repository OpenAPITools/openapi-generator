package org.openapitools.codegen.cppboostbeast;
import org.openapitools.codegen.meta.features.DataTypeFeature;
import org.openapitools.codegen.meta.FeatureSet;
import org.openapitools.codegen.languages.CppBoostBeastClientCodegen;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

public class CppBoostBeastClientApiCodegenTest {

    @Test
    public void generatesRepeatedQueryKeysForOpenApi2MultiCollections() throws IOException {
        Path testOutputRoot = Files.createDirectories(Path.of("target"));
        Path generatedClientDirectory = Files.createTempDirectory(
                testOutputRoot, "cpp-boost-beast-oas2-multi-regression");

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/2_0/cpp-boost-beast-client/api-collection-format-regression.yaml")
                .setOutputDir(generatedClientDirectory.toString());

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        String generatedApiSource = Files.readString(
                generatedClientDirectory.resolve(Path.of("api", "CollectionFormatApi.cpp")));
        String getItemsMethod = extractMethod(
                generatedApiSource, "CollectionFormatApi::getItems(");
        // Wave 5.1: the OAS2 multi collection (form style, explode=true) is
        // emitted through the unified style-aware appender.
        assertTrue(getItemsMethod.contains("appendParamQueryParameter("));
        assertTrue(getItemsMethod.contains("\"ids\","));
        assertTrue(getItemsMethod.contains("ids,"));
        assertTrue(getItemsMethod.contains("\"form\","));
        assertTrue(getItemsMethod.contains("true,"));
    }

    @Test
    public void generatesSafeParameterSerializationAndResponseHandling() throws IOException {
        Path testOutputRoot = Files.createDirectories(Path.of("target"));
        Path generatedClientDirectory = Files.createTempDirectory(
                testOutputRoot, "cpp-boost-beast-api-regression");

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_0/cpp-boost-beast-client/api-generation-regression.yaml")
                .setOutputDir(generatedClientDirectory.toString());

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        Path generatedApiPath = generatedClientDirectory.resolve(Path.of("api", "RegressionApi.cpp"));
        Path generatedApiHeaderPath = generatedClientDirectory.resolve(Path.of("api", "RegressionApi.h"));
        String generatedApiSource = Files.readString(generatedApiPath);
        String generatedApiHeader = Files.readString(generatedApiHeaderPath);

        assertTrue(generatedApiHeader.contains("#include <boost/optional.hpp>"));
        assertTrue(generatedApiHeader.contains("#include <utility>"));
        assertTrue(generatedApiHeader.contains("std::shared_ptr<HttpClient> client,"));
        assertTrue(generatedApiHeader.contains("m_client(std::move(client))"));

        String defaultOnlyMethod = extractMethod(generatedApiSource, "RegressionApi::getDefaultOnly(");
        assertTrue(defaultOnlyMethod.contains("ResponseBodyDeserializer<std::int32_t>::deserialize("));
        assertFalse(defaultOnlyMethod.contains("throw RegressionApiException"));

        String explicitThenDefaultMethod = extractMethod(generatedApiSource, "RegressionApi::getDefaultAfterExplicit(");
        int explicitResponsePosition = explicitThenDefaultMethod.indexOf("boost::beast::http::status(200)");
        assertTrue(explicitResponsePosition >= 0);
        // Phase 0-9: response-union path returns GetDefaultAfterExplicitResponse
        // instead of throwing for the fallback. The explicit 200 branch is
        // handled first, then the default branch deserializes std::string.
        assertTrue(explicitThenDefaultMethod.contains("executeWithMetadata"),
                "Response-union method must use executeWithMetadata");
        // 200 branch must deserialize std::int32_t before the default fallthrough
        assertTrue(explicitThenDefaultMethod.contains("std::int32_t{"),
                "200 branch must deserialize std::int32_t");
        // Default branch deserializes std::string (no throw) at the end
        assertTrue(explicitThenDefaultMethod.indexOf("std::string{") > explicitResponsePosition,
                "Default branch must deserialize std::string after the 200 branch");
        // No throw in the default branch (response union handles fallback)
        assertFalse(explicitThenDefaultMethod.contains("throw RegressionApiException"),
                "Default branch must not throw (response union handles fallback)");

        String voidDefaultMethod = extractMethod(generatedApiSource, "RegressionApi::getVoidDefault(");
        assertFalse(voidDefaultMethod.contains("Bodyless fallback"));

        String emptyMapMethod = extractMethod(generatedApiSource, "RegressionApi::getEmptyMap(");
        assertTrue(emptyMapMethod.contains(
                "ResponseBodyDeserializer<std::map<std::string, std::string>>::deserialize("));
        assertTrue(emptyMapMethod.contains("responseBody,\n            responseContentType,\n            true);"));

        String emptyFreeFormMethod = extractMethod(generatedApiSource, "RegressionApi::getEmptyFreeForm(");
        assertTrue(emptyFreeFormMethod.contains(
                "ResponseBodyDeserializer<boost::json::value>::deserialize("));
        assertTrue(emptyFreeFormMethod.contains("responseBody,\n            responseContentType,\n            true);"));

        String rawTextMethod = extractMethod(generatedApiSource, "RegressionApi::postRawText(");
        assertTrue(rawTextMethod.contains("headers[\"Content-Type\"] = requestContentType;"));
        assertTrue(rawTextMethod.contains("serializedRequestBody = toRawBodyValue(body);"));
        assertTrue(rawTextMethod.indexOf("requestContentType = selectPreferredContentType")
                < rawTextMethod.indexOf("if (isJsonContentType(requestContentType))"));

        String rawBinaryMethod = extractMethod(generatedApiSource, "RegressionApi::postRawBinary(");
        assertTrue(rawBinaryMethod.contains("serializedRequestBody = toRawBodyValue(body);"));

        String rawIntegerMethod = extractMethod(generatedApiSource, "RegressionApi::postRawInteger(");
        assertTrue(rawIntegerMethod.contains(
                "serializedRequestBody = toRawBodyValue(body);"));

        String rawBooleanMethod = extractMethod(generatedApiSource, "RegressionApi::postRawBoolean(");
        assertTrue(rawBooleanMethod.contains(
                "serializedRequestBody = toRawBodyValue(body);"));

        String rawXmlMethod = extractMethod(generatedApiSource, "RegressionApi::postRawXml(");
        assertTrue(rawXmlMethod.contains("serializedRequestBody = toRawBodyValue(body);"));

        String structuredTextMethod = extractMethod(generatedApiSource, "RegressionApi::postStructuredText(");
        assertTrue(structuredTextMethod.contains(
                "does not support structured request bodies"));

        String structuredXmlRequestMethod = extractMethod(
                generatedApiSource, "RegressionApi::postStructuredXml(");
        assertTrue(structuredXmlRequestMethod.contains(
                "does not support structured request bodies"));

        String structuredXmlResponseMethod = extractMethod(
                generatedApiSource, "RegressionApi::getStructuredXml(");
        assertTrue(structuredXmlResponseMethod.contains(
                "responseContentType = selectPreferredContentType(acceptTypes);"));
        assertTrue(structuredXmlResponseMethod.contains(
                "ResponseBodyDeserializer<std::shared_ptr<Payload>>::deserialize("));

        String rawXmlResponseMethod = extractMethod(generatedApiSource, "RegressionApi::getRawXml(");
        assertTrue(rawXmlResponseMethod.contains(
                "ResponseBodyDeserializer<std::string>::deserialize("));
        assertTrue(rawXmlResponseMethod.contains("responseContentType,"));

        String jsonPreferredResponseMethod = extractMethod(
                generatedApiSource, "RegressionApi::getJsonPreferred(");
        assertTrue(jsonPreferredResponseMethod.contains("\"application/xml\""));
        assertTrue(jsonPreferredResponseMethod.contains("\"application/json\""));
        assertTrue(jsonPreferredResponseMethod.contains(
                "responseContentType = selectPreferredContentType(acceptTypes);"));
        assertTrue(generatedApiSource.contains(
                "static const std::array<std::string, 1> preferredTypes = {\"json\"};"));
        assertTrue(generatedApiSource.contains(
                "does not support structured response bodies"));

        String multipartMethod = extractMethod(generatedApiSource, "RegressionApi::postMultipartForm(");
        assertTrue(multipartMethod.contains("selectMultipartBoundary(formParameters)"));
        assertTrue(multipartMethod.contains(
                "headers[\"Content-Type\"] = requestContentType + \"; boundary=\" + multipartBoundary;"));
        assertTrue(multipartMethod.contains("serializeMultipartFormData(formParameters, multipartBoundary)"));
        assertTrue(multipartMethod.contains("\"file\","));
        assertTrue(multipartMethod.contains("true,\n        \"application/octet-stream\""),
                "Binary file must pass true and application/octet-stream as 4th arg");
        assertTrue(generatedApiHeader.contains("const std::string& fileFilename = \"file\""),
                "Binary multipart inputs must expose a caller-controlled filename");
        assertTrue(generatedApiHeader.contains(
                "const boost::optional<std::string>& description"),
                "Optional multipart inputs must expose an omitted state");
        assertTrue(generatedApiHeader.contains(
                "const boost::optional<std::string>& file"),
                "Optional binary multipart inputs must expose an omitted state");
        assertTrue(generatedApiHeader.contains(
                "const std::string& description, const std::string& file,"
                + " const std::string& fileFilename = \"file\") {"),
                "Value-typed multipart calls must remain source-compatible");
        assertTrue(generatedApiHeader.contains(
                "boost::optional<std::string>(description),"
                + " boost::optional<std::string>(file), fileFilename);"),
                "Value-typed multipart overloads must forward to omission-aware storage");
        assertTrue(multipartMethod.contains(
                "\"application/octet-stream\",\n        fileFilename)"),
                "Binary multipart inputs must pass the caller-controlled filename");
        assertTrue(multipartMethod.contains("if (hasFormParameterValue(description))"),
                "Optional multipart inputs must be omitted when their storage is disengaged");
        assertTrue(multipartMethod.contains("toFormParameterValue(*description)"),
                "Engaged optional multipart values must serialize their contained value");
        assertTrue(generatedApiSource.contains("std::string filename;"));
        assertTrue(generatedApiSource.contains(
                "escapeMultipartParameter(formParameter.filename)"));
        assertTrue(generatedApiSource.contains("attempt < 16"),
                "Multipart boundary collision retries must remain bounded");
        assertTrue(generatedApiSource.contains(
                "validateMultipartHeaderValue(formParameter.contentType)"),
                "Multipart content types must reject header injection");

        String urlEncodedMethod = extractMethod(generatedApiSource, "RegressionApi::postUrlEncodedForm(");
        assertTrue(urlEncodedMethod.contains("serializeUrlEncodedFormData(formParameters)"));
        assertTrue(urlEncodedMethod.contains("toFormParameterValue(*enabled)"));
        assertTrue(generatedApiSource.contains(
                "inline std::string toFormParameterValue(bool value)"));

        String queryEncodingMethod = extractMethod(generatedApiSource, "RegressionApi::getQueryEncoding(");
        assertTrue(generatedApiHeader.contains(
                "const boost::optional<std::string>& optionalValue"));
        // Wave 5.1: every query param rides the unified style-aware appender
        // (name, value, style, explode, allowReserved, allowEmptyValue).
        assertTrue(queryEncodingMethod.contains("appendParamQueryParameter("));
        assertTrue(queryEncodingMethod.contains("\"wire-name\","));
        assertTrue(queryEncodingMethod.contains("\"form\","));
        // form/explode=true (multi) array cell
        assertTrue(queryEncodingMethod.contains(
                "\"values\",\n            values,"));
        assertTrue(queryEncodingMethod.contains(
                "values,\n            \"form\",\n            true,"));
        // form/explode=false (csv) cells
        assertTrue(queryEncodingMethod.contains(
                "csvValues,\n            \"form\",\n            false,"));
        assertTrue(queryEncodingMethod.contains(
                "explicitFormDefaultValues,\n            \"form\",\n            true,"));
        assertTrue(queryEncodingMethod.contains(
                "noStyleCsvValues,\n            \"form\",\n            false,"));
        // form/explode=true (multi) cell without explicit style keys
        assertTrue(queryEncodingMethod.contains(
                "noStyleExplodedValues,\n            \"form\",\n            true,"));
        // spaceDelimited / pipeDelimited cells (explode=false)
        assertTrue(queryEncodingMethod.contains(
                "spaceValues,\n            \"spaceDelimited\",\n            false,"));
        assertTrue(queryEncodingMethod.contains(
                "pipeValues,\n            \"pipeDelimited\",\n            false,"));
        // optional gate + deref
        assertTrue(queryEncodingMethod.contains("if (optionalValue)"));
        assertTrue(queryEncodingMethod.contains(
                "*optionalValue,"));
        // exploded-map cell (form/explode=true map)
        assertTrue(queryEncodingMethod.contains(
                "metadata,\n            \"form\",\n            true,"));
        // csv-map cell (form/explode=false map)
        assertTrue(queryEncodingMethod.contains(
                "compactMetadata,\n            \"form\",\n            false,"));
        // deepObject cell
        assertTrue(queryEncodingMethod.contains(
                "\"scoped-metadata\",\n            scopedMetadata,"));
        assertTrue(queryEncodingMethod.contains(
                "scopedMetadata,\n            \"deepObject\",\n            true,"));
        // header cells (simple style, explode=false default)
        assertTrue(queryEncodingMethod.contains(
                "serializeHeaderParameterValue(wireHeader, false)"));
        assertTrue(queryEncodingMethod.contains(
                "serializeHeaderParameterValue(arrayHeader, false)"));
        assertTrue(queryEncodingMethod.contains(
                "serializeHeaderParameterValue(objectHeader, false)"));
        assertFalse(queryEncodingMethod.contains("\"wireName=\""));
        // The old fail-closed header guard is gone: object headers now
        // serialize as k=v pairs instead of throwing at runtime.
        assertFalse(generatedApiSource.contains(
                "Header parameter serialization supports only primitive values and arrays of primitive values"));

        String pathEncodingMethod = extractMethod(generatedApiSource, "RegressionApi::getPathEncoding(");
        assertTrue(pathEncodingMethod.contains(
                "replacePathParameter(path, \"atomicValue\", atomicValue, \"simple\", false);"));
        assertTrue(pathEncodingMethod.contains(
                "replacePathParameter(path, \"pathValues\", pathValues, \"simple\", false);"));
        assertFalse(pathEncodingMethod.contains("boost::format"));
        assertTrue(pathEncodingMethod.contains(
                "operationServerPrefix(m_context, \"\") + \"/path-encoding/%20/{pathValues}/{atomicValue}\""));
        assertTrue(generatedApiSource.contains("path.find(placeholder, position)"));
        assertTrue(generatedApiSource.contains("percentEncodePathValue"));

        String unexpectedTypedMethod = extractMethod(
                generatedApiSource, "RegressionApi::getUnexpectedTyped(");
        assertTrue(unexpectedTypedMethod.contains(
                "throw RegressionApiException(statusCode, \"Unexpected HTTP status code\");"));
        assertTrue(unexpectedTypedMethod.indexOf("boost::beast::http::status(200)")
                < unexpectedTypedMethod.indexOf("Unexpected HTTP status code"));

        String unexpectedVoidMethod = extractMethod(
                generatedApiSource, "RegressionApi::deleteUnexpectedVoid(");
        assertTrue(unexpectedVoidMethod.contains(
                "throw RegressionApiException(statusCode, \"Unexpected HTTP status code\");"));
        assertTrue(unexpectedVoidMethod.indexOf("boost::beast::http::status(204)")
                < unexpectedVoidMethod.indexOf("Unexpected HTTP status code"));

        String responseRangeMethod = extractMethod(
                generatedApiSource, "RegressionApi::getResponseRange(");
        int rangeExplicitResponsePosition = responseRangeMethod.indexOf(
                "statusCode == boost::beast::http::status(200)");
        int rangeResponsePosition = responseRangeMethod.indexOf(
                "static_cast<unsigned int>(statusCode) / 100U == 2U");
        assertTrue(rangeExplicitResponsePosition >= 0);
        assertTrue(rangeResponsePosition > rangeExplicitResponsePosition);
        assertFalse(responseRangeMethod.contains("status(2XX)"));

        assertEquals(countOccurrences(generatedApiSource, "RegressionApi::getSamePath("), 1);
        assertEquals(countOccurrences(generatedApiSource, "RegressionApi::postSamePath("), 1);
        String getSamePathMethod = extractMethod(generatedApiSource, "RegressionApi::getSamePath(");
        String postSamePathMethod = extractMethod(generatedApiSource, "RegressionApi::postSamePath(");
        assertTrue(getSamePathMethod.contains("m_client->execute(\"GET\""));
        assertTrue(postSamePathMethod.contains("m_client->execute(\"POST\""));
    }

    private static String extractMethod(String generatedApiSource, String methodSignature) {
        int methodStart = generatedApiSource.indexOf(methodSignature);
        assertTrue(methodStart >= 0, "Missing generated method: " + methodSignature);
        int methodEnd = generatedApiSource.indexOf("\n}", methodStart);
        assertTrue(methodEnd > methodStart, "Missing closing brace for generated method: " + methodSignature);
        return generatedApiSource.substring(methodStart, methodEnd);
    }

    private static int countOccurrences(String source, String expectedText) {
        int occurrenceCount = 0;
        int searchPosition = 0;
        while ((searchPosition = source.indexOf(expectedText, searchPosition)) >= 0) {
            occurrenceCount++;
            searchPosition += expectedText.length();
        }
        return occurrenceCount;
    }

    @Test
    public void mProfileDestinationDomainsAndFeatureSet() throws IOException {
        // Wave-M (GM3 contract surface + M-audit): the generated destination
        // domains and the FeatureSet DataTypeFeature declaration must match
        // the M-corpus evidence.  Also asserts the F3 finite-check emission
        // (non-finite float/double destinations become representation
        // diagnostics, never silent).
        Path testOutputRoot = Files.createDirectories(Path.of("target"));
        Path generatedClientDirectory = Files.createTempDirectory(
                testOutputRoot, "cpp-boost-beast-m-profile");

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/fixtures/m-probe-schemas.yaml")
                .setOutputDir(generatedClientDirectory.toString())
                .addAdditionalProperty("apiPackage", "api")
                .addAdditionalProperty("modelPackage", "model");

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        String int32 = Files.readString(generatedClientDirectory.resolve(Path.of("model", "Int32Box.h")));
        String int64 = Files.readString(generatedClientDirectory.resolve(Path.of("model", "Int64Box.h")));
        String floatH = Files.readString(generatedClientDirectory.resolve(Path.of("model", "FloatBox.h")));
        String doubleH = Files.readString(generatedClientDirectory.resolve(Path.of("model", "DoubleBox.h")));
        String enumH = Files.readString(generatedClientDirectory.resolve(Path.of("model", "EnumBox.h")));
        String nullableH = Files.readString(generatedClientDirectory.resolve(Path.of("model", "NullableBox.h")));
        String anyH = Files.readString(generatedClientDirectory.resolve(Path.of("model", "AnyType.h")));
        String polyH = Files.readString(generatedClientDirectory.resolve(Path.of("model", "PolyBox.h")));

        assertTrue(int32.contains("std::int32_t"), "int32 destination domain");
        assertTrue(int64.contains("std::int64_t"), "int64 destination domain");
        assertTrue(floatH.contains("float"), "float destination domain");
        assertTrue(doubleH.contains("double"), "double destination domain");
        assertTrue(enumH.contains("std::string"),
                "enum destination domain (open member type, closed validation)");
        assertTrue(nullableH.contains("NullableField"),
                "3.1 type-array null destination -> tri-state NullableField");
        assertTrue(anyH.contains("using AnyType = boost::json::value"),
                "AnyType raw fallback destination");
        assertTrue(polyH.contains("std::variant"), "oneOf union destination");

        // F3: non-finite destinations must throw a representation
        // diagnostic — the emitted converter carries the finite check.
        String floatCpp = Files.readString(generatedClientDirectory.resolve(Path.of("model", "FloatBox.cpp")));
        String doubleCpp = Files.readString(generatedClientDirectory.resolve(Path.of("model", "DoubleBox.cpp")));
        assertTrue(floatCpp.contains("non-finite destination"), "float finite-check emitted");
        assertTrue(doubleCpp.contains("non-finite destination"), "double finite-check emitted");

        // M-audit: FeatureSet declares exactly the corpus-proven domains.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        java.util.Set<DataTypeFeature> types = codegen.getFeatureSet().getDataTypeFeatures();
        for (DataTypeFeature expected : java.util.Arrays.asList(
                DataTypeFeature.Int32, DataTypeFeature.Int64, DataTypeFeature.Float,
                DataTypeFeature.Double, DataTypeFeature.String, DataTypeFeature.Boolean,
                DataTypeFeature.Enum, DataTypeFeature.Array, DataTypeFeature.Maps,
                DataTypeFeature.Object, DataTypeFeature.Null, DataTypeFeature.AnyType)) {
            assertTrue(types.contains(expected),
                    "DataTypeFeature " + expected + " must be declared (corpus-proven)");
        }
        assertFalse(types.contains(DataTypeFeature.Decimal),
                "Decimal must NOT be declared (no decimal destination domain)");
        assertFalse(types.contains(DataTypeFeature.Uuid),
                "Uuid must NOT be declared (uuid maps to the string destination)");
    }

    @Test
    public void formatDestinationsMapToStringOrDoubleAndFeatureSetStaysClean() throws IOException {
        // Gap-5 closure: OAS 3.1 string formats (date-time/date/uuid/byte/
        // binary/password) map to the std::string destination, `decimal`
        // maps to double, and NO format-specific DataTypeFeature entries
        // (Date/DateTime/Uuid/Byte/Binary/Password/Decimal) are declared —
        // formats are annotations (2020-12 Format-Annotation default), so
        // they need no distinct C++ destination.
        Path testOutputRoot = Files.createDirectories(Path.of("target"));
        Path generatedClientDirectory = Files.createTempDirectory(
                testOutputRoot, "cpp-boost-beast-format-destinations");

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_1/cpp-boost-beast-client/format-destinations.yaml")
                .setOutputDir(generatedClientDirectory.toString())
                .addAdditionalProperty("apiPackage", "api")
                .addAdditionalProperty("modelPackage", "model");

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        // Every string-format box must be a std::string destination.
        for (String box : java.util.Arrays.asList(
                "DateTimeBox", "DateBox", "UuidBox", "ByteBox", "BinaryBox", "PasswordBox")) {
            String header = Files.readString(
                    generatedClientDirectory.resolve(Path.of("model", box + ".h")));
            assertTrue(header.contains("std::string m_v"),
                    box + " must map to the std::string destination");
            assertFalse(header.contains(" double m_v"),
                    box + " must not be a double destination");
        }
        // decimal maps to double (no distinct decimal destination).
        String decimal = Files.readString(
                generatedClientDirectory.resolve(Path.of("model", "DecimalBox.h")));
        assertTrue(decimal.contains("double m_v"),
                "decimal must map to the double destination");

        // FeatureSet stays clean: NO format-specific data-type features.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        java.util.Set<DataTypeFeature> types = codegen.getFeatureSet().getDataTypeFeatures();
        for (DataTypeFeature forbidden : java.util.Arrays.asList(
                DataTypeFeature.Date, DataTypeFeature.DateTime, DataTypeFeature.Uuid,
                DataTypeFeature.Byte, DataTypeFeature.Binary, DataTypeFeature.Password,
                DataTypeFeature.Decimal)) {
            assertFalse(types.contains(forbidden),
                    "format-specific DataTypeFeature " + forbidden + " must NOT be declared");
        }
    }
}
