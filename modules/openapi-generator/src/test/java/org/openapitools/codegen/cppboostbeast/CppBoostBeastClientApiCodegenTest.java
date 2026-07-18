package org.openapitools.codegen.cppboostbeast;

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
        assertTrue(getItemsMethod.contains("appendMultiQueryParameters("));
        assertTrue(getItemsMethod.contains("\"ids\",\n            ids);"));
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
        assertTrue(defaultOnlyMethod.contains("ResponseBodyDeserializer<int32_t>::deserialize("));
        assertFalse(defaultOnlyMethod.contains("throw RegressionApiException"));

        String explicitThenDefaultMethod = extractMethod(generatedApiSource, "RegressionApi::getDefaultAfterExplicit(");
        int explicitResponsePosition = explicitThenDefaultMethod.indexOf("boost::beast::http::status(200)");
        int defaultResponsePosition = explicitThenDefaultMethod.indexOf("Incompatible fallback");
        assertTrue(explicitResponsePosition >= 0);
        assertTrue(defaultResponsePosition > explicitResponsePosition);
        assertTrue(explicitThenDefaultMethod.contains(
                "throw RegressionApiException(statusCode, \"Incompatible fallback\");"));

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
        assertTrue(multipartMethod.contains("true);"));

        String urlEncodedMethod = extractMethod(generatedApiSource, "RegressionApi::postUrlEncodedForm(");
        assertTrue(urlEncodedMethod.contains("serializeUrlEncodedFormData(formParameters)"));
        assertTrue(urlEncodedMethod.contains("toFormParameterValue(enabled)"));
        assertTrue(generatedApiSource.contains(
                "inline std::string toFormParameterValue(bool value)"));

        String queryEncodingMethod = extractMethod(generatedApiSource, "RegressionApi::getQueryEncoding(");
        assertTrue(generatedApiHeader.contains(
                "const boost::optional<std::string>& optionalValue"));
        assertTrue(queryEncodingMethod.contains("appendQueryParameter("));
        assertTrue(queryEncodingMethod.contains("\"wire-name\","));
        assertTrue(queryEncodingMethod.contains("serializeQueryParameterValue(wireName)"));
        assertTrue(queryEncodingMethod.contains("appendMultiQueryParameters("));
        assertTrue(queryEncodingMethod.contains("\"values\",\n            values);"));
        assertTrue(queryEncodingMethod.contains("csvValues,\n                \",\""));
        assertTrue(queryEncodingMethod.contains("noStyleCsvValues,\n                \",\""));
        assertTrue(queryEncodingMethod.contains("spaceValues,\n                \"%20\""));
        assertTrue(queryEncodingMethod.contains("pipeValues,\n                \"%7C\""));
        assertTrue(queryEncodingMethod.contains("if (optionalValue)"));
        assertTrue(queryEncodingMethod.contains("serializeQueryParameterValue(*optionalValue)"));
        assertTrue(queryEncodingMethod.contains(
                "serializeHeaderParameterValue(wireHeader)"));
        assertTrue(queryEncodingMethod.contains(
                "serializeHeaderParameterValue(arrayHeader)"));
        assertTrue(queryEncodingMethod.contains(
                "serializeHeaderParameterValue(objectHeader)"));
        assertFalse(queryEncodingMethod.contains("\"wireName=\""));
        assertFalse(queryEncodingMethod.contains("serializeQueryParameterValue(values);"));
        assertTrue(generatedApiSource.contains(
                "Header parameter serialization supports only primitive values and arrays of primitive values"));

        String pathEncodingMethod = extractMethod(generatedApiSource, "RegressionApi::getPathEncoding(");
        assertTrue(pathEncodingMethod.contains("serializePathParameterValue(atomicValue)"));
        assertTrue(pathEncodingMethod.contains("serializePathParameterValue(pathValues)"));
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
}
