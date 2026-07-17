package org.openapitools.codegen.cppboostbeast;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.Test;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

public class CppBoostBeastClientApiCodegenTest {

    @Test
    public void generatesContentTypeAwareBodiesAndResponseFallbacks() throws IOException {
        Path testOutputRoot = Files.createDirectories(Path.of("target"));
        Path generatedClientDirectory = Files.createTempDirectory(
                testOutputRoot, "cpp-boost-beast-api-regression");

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("cpp-boost-beast-client")
                .setInputSpec("src/test/resources/3_0/cpp-boost-beast-client/api-generation-regression.yaml")
                .setOutputDir(generatedClientDirectory.toString());

        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

        Path generatedApiPath = generatedClientDirectory.resolve(Path.of("api", "RegressionApi.cpp"));
        String generatedApiSource = Files.readString(generatedApiPath);

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
        assertTrue(emptyMapMethod.contains("responseBody,\n            true);"));

        String emptyFreeFormMethod = extractMethod(generatedApiSource, "RegressionApi::getEmptyFreeForm(");
        assertTrue(emptyFreeFormMethod.contains(
                "ResponseBodyDeserializer<boost::json::value>::deserialize("));
        assertTrue(emptyFreeFormMethod.contains("responseBody,\n            true);"));

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
        assertTrue(queryEncodingMethod.contains("percentEncodeQueryValue(\"wire-name\")"));
        assertTrue(queryEncodingMethod.contains("serializeQueryParameterValue(wireName)"));
        assertTrue(queryEncodingMethod.contains("serializeQueryParameterValue(values)"));
        assertTrue(queryEncodingMethod.contains("std::make_pair(\"wire-header\""));
        assertFalse(queryEncodingMethod.contains("\"wireName=\""));
    }

    private static String extractMethod(String generatedApiSource, String methodSignature) {
        int methodStart = generatedApiSource.indexOf(methodSignature);
        assertTrue(methodStart >= 0, "Missing generated method: " + methodSignature);
        int methodEnd = generatedApiSource.indexOf("\n}", methodStart);
        assertTrue(methodEnd > methodStart, "Missing closing brace for generated method: " + methodSignature);
        return generatedApiSource.substring(methodStart, methodEnd);
    }
}
