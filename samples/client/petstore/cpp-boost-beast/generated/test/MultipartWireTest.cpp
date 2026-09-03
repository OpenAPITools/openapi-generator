// -----------------------------------------------------------------------------
// Wire-level multipart encoding regression executable.
// Exercises the shared serializer used by generated API implementations. Run via
// CTest:
//   cmake -DMULTIPART_ENCODING_SELF_TEST=ON -B build
//   cmake --build build --parallel
//   ctest --test-dir build --output-on-failure
// -----------------------------------------------------------------------------

#include <cstdint>
#include <cstdlib>
#include <exception>
#include <iostream>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

#include <boost/optional.hpp>

#include "../api/HttpClient.h"

namespace org {
namespace openapitools {
namespace client {
namespace api {

namespace {

[[noreturn]] void fail(const std::string& message) {
    throw std::runtime_error(message);
}

void require(bool condition, const std::string& message) {
    if (!condition) {
        fail(message);
    }
}

struct MultipartPart {
    std::string contentDisposition;
    std::string contentType;
    std::string payload;
};

std::vector<MultipartPart> parseMultipartBody(
        const std::string& wire, const std::string& boundary) {
    const std::string delimiter = "--" + boundary;
    std::vector<MultipartPart> parts;
    std::size_t offset = 0;

    while (true) {
        require(wire.compare(offset, delimiter.size(), delimiter) == 0,
                "multipart part must begin with its boundary delimiter");
        offset += delimiter.size();
        if (wire.compare(offset, 4, "--\r\n") == 0) {
            offset += 4;
            require(offset == wire.size(), "multipart data must end after closing boundary");
            return parts;
        }
        require(wire.compare(offset, 2, "\r\n") == 0,
                "multipart boundary must be followed by CRLF");
        offset += 2;

        const std::size_t headerEnd = wire.find("\r\n\r\n", offset);
        require(headerEnd != std::string::npos, "multipart part must terminate headers");
        MultipartPart part;
        std::size_t headerOffset = offset;
        while (headerOffset < headerEnd) {
            const std::size_t lineEnd = wire.find("\r\n", headerOffset);
            require(lineEnd != std::string::npos && lineEnd <= headerEnd,
                    "multipart header line must terminate with CRLF");
            const std::size_t colon = wire.find(':', headerOffset);
            require(colon != std::string::npos && colon < lineEnd,
                    "multipart header must have a name and value");
            const std::string headerName = wire.substr(headerOffset, colon - headerOffset);
            const std::string headerValue = wire.substr(colon + 1, lineEnd - colon - 1);
            require(!headerValue.empty() && headerValue.front() == ' ',
                    "multipart header values must have one leading space");
            if (headerName == "Content-Disposition") {
                part.contentDisposition = headerValue.substr(1);
            } else if (headerName == "Content-Type") {
                part.contentType = headerValue.substr(1);
            }
            headerOffset = lineEnd + 2;
        }
        require(!part.contentDisposition.empty(),
                "multipart part must have Content-Disposition");

        const std::size_t payloadOffset = headerEnd + 4;
        const std::size_t nextBoundary = wire.find("\r\n" + delimiter, payloadOffset);
        require(nextBoundary != std::string::npos,
                "multipart part payload must be followed by a boundary delimiter");
        part.payload = wire.substr(payloadOffset, nextBoundary - payloadOffset);
        parts.emplace_back(std::move(part));
        offset = nextBoundary + 2;
    }
}

void requirePart(const std::vector<MultipartPart>& parts, std::size_t index,
                 const std::string& disposition, const std::string& contentType,
                 const std::string& payload) {
    require(index < parts.size(), "multipart part index must exist");
    const MultipartPart& part = parts[index];
    require(part.contentDisposition == disposition,
            "multipart Content-Disposition must match the encoded field");
    require(part.contentType == contentType,
            "multipart Content-Type must match the encoded field");
    require(part.payload == payload, "multipart payload must match the encoded field");
}

void testExplicitEncoding() {
    const std::string boundary = "==BOUNDARY==";
    std::vector<FormParameter> parameters;
    parameters.emplace_back("avatar", "\x89PNG\r\n\x1a\n", true, "image/png", "avatar");
    parameters.emplace_back("report", "%PDF-1.4", true, "application/pdf", "report");

    const std::vector<MultipartPart> parts = parseMultipartBody(
            serializeMultipartFormData(parameters, boundary), boundary);
    require(parts.size() == 2, "explicit encoding must produce both parts");
    requirePart(parts, 0, "form-data; name=\"avatar\"; filename=\"avatar\"",
                "image/png", "\x89PNG\r\n\x1a\n");
    requirePart(parts, 1, "form-data; name=\"report\"; filename=\"report\"",
                "application/pdf", "%PDF-1.4");

    parameters.clear();
    parameters.emplace_back("description", "A description", false);
    const std::vector<MultipartPart> stringParts = parseMultipartBody(
            serializeMultipartFormData(parameters, boundary), boundary);
    require(stringParts.size() == 1, "a text field must produce one part");
    requirePart(stringParts, 0, "form-data; name=\"description\"", "", "A description");
}

void testDefaultContentTypes() {
    const std::string boundary = "==BOUNDARY==";

    std::vector<FormParameter> textParameters;
    textParameters.emplace_back("textContent", "Hello", false, "text/plain");
    const std::vector<MultipartPart> textParts = parseMultipartBody(
            serializeMultipartFormData(textParameters, boundary), boundary);
    requirePart(textParts, 0, "form-data; name=\"textContent\"", "text/plain", "Hello");

    std::vector<FormParameter> arrayParameters;
    arrayParameters.emplace_back("tags", "a,b,c", false, "text/plain");
    const std::vector<MultipartPart> arrayParts = parseMultipartBody(
            serializeMultipartFormData(arrayParameters, boundary), boundary);
    requirePart(arrayParts, 0, "form-data; name=\"tags\"", "text/plain", "a,b,c");

    std::vector<FormParameter> jsonParameters;
    jsonParameters.emplace_back("payload", "{\"key\":\"value\"}", false,
                                "application/json");
    jsonParameters.emplace_back("items", "[{\"id\":1},{\"id\":2}]", false,
                                "application/json");
    const std::vector<MultipartPart> jsonParts = parseMultipartBody(
            serializeMultipartFormData(jsonParameters, boundary), boundary);
    require(jsonParts.size() == 2, "JSON fields must produce both parts");
    requirePart(jsonParts, 0, "form-data; name=\"payload\"", "application/json",
                "{\"key\":\"value\"}");
    requirePart(jsonParts, 1, "form-data; name=\"items\"", "application/json",
                "[{\"id\":1},{\"id\":2}]");

    std::vector<FormParameter> binaryParameters;
    binaryParameters.emplace_back("rawData", "binary-payload", true,
                                  "application/octet-stream", "rawData");
    const std::vector<MultipartPart> binaryParts = parseMultipartBody(
            serializeMultipartFormData(binaryParameters, boundary), boundary);
    requirePart(binaryParts, 0, "form-data; name=\"rawData\"; filename=\"rawData\"",
                "application/octet-stream", "binary-payload");
}

void testContentDispositionAndMixedEncoding() {
    const std::string boundary = "==BOUNDARY==";
    std::vector<FormParameter> parameters;
    parameters.emplace_back("fieldA", "valueA", false);
    parameters.emplace_back("fieldB", "valueB", true, "image/png", "photo.png");
    parameters.emplace_back("fieldC", "valueC", true, "image/png", "fieldC");
    parameters.emplace_back("fieldD", "valueD", true, "image/png", "");
    parameters.emplace_back("signature", "sig", true, "application/octet-stream", "signature");

    const std::vector<MultipartPart> parts = parseMultipartBody(
            serializeMultipartFormData(parameters, boundary), boundary);
    require(parts.size() == 5, "mixed encoding must preserve all field parts");
    requirePart(parts, 0, "form-data; name=\"fieldA\"", "", "valueA");
    requirePart(parts, 1, "form-data; name=\"fieldB\"; filename=\"photo.png\"",
                "image/png", "valueB");
    requirePart(parts, 2, "form-data; name=\"fieldC\"; filename=\"fieldC\"",
                "image/png", "valueC");
    requirePart(parts, 3, "form-data; name=\"fieldD\"; filename=\"\"",
                "image/png", "valueD");
    requirePart(parts, 4, "form-data; name=\"signature\"; filename=\"signature\"",
                "application/octet-stream", "sig");
}

void testHeaderInjectionRejected() {
    std::vector<FormParameter> filenameParameters;
    filenameParameters.emplace_back(
            "file", "value", true, "application/octet-stream", "bad\r\nname");
    bool filenameRejected = false;
    try {
        (void)serializeMultipartFormData(filenameParameters, "==BOUNDARY==");
    } catch (const std::invalid_argument&) {
        filenameRejected = true;
    }
    require(filenameRejected, "multipart serializer must reject CRLF in filenames");

    std::vector<FormParameter> contentTypeParameters;
    contentTypeParameters.emplace_back(
            "file", "value", true, "text/plain\r\nX-Evil: yes", "file");
    bool contentTypeRejected = false;
    try {
        (void)serializeMultipartFormData(contentTypeParameters, "==BOUNDARY==");
    } catch (const std::invalid_argument&) {
        contentTypeRejected = true;
    }
    require(contentTypeRejected, "multipart serializer must reject CRLF in content types");
}

void testBoundarySelectionAndWireStructure() {
    std::vector<FormParameter> ordinaryParameters;
    ordinaryParameters.emplace_back("data", "value", false);
    require(selectMultipartBoundary(ordinaryParameters) == "OpenAPIGeneratorBoundary",
            "a non-colliding payload must retain the stable default boundary");

    std::vector<FormParameter> collidingParameters;
    collidingParameters.emplace_back("data", "OpenAPIGeneratorBoundary", true, "", "data");
    const std::string selectedBoundary = selectMultipartBoundary(collidingParameters);
    require(selectedBoundary != "OpenAPIGeneratorBoundary",
            "multipart boundary must not appear in a payload");
    require(selectedBoundary.find("OpenAPIGeneratorBoundary") == 0,
            "replacement multipart boundary must retain its stable prefix");
    require(selectedBoundary.size() <= 70,
            "multipart boundary must respect the RFC 2046 length limit");

    const std::string boundary = "TESTBOUNDARY";
    std::vector<FormParameter> parameters;
    parameters.emplace_back("field", "value", false);
    const std::string wire = serializeMultipartFormData(parameters, boundary);
    const std::vector<MultipartPart> parts = parseMultipartBody(wire, boundary);
    require(parts.size() == 1, "wire structure must contain one parsed part");
    requirePart(parts, 0, "form-data; name=\"field\"", "", "value");
}

} // namespace

} // namespace org
} // namespace openapitools
} // namespace client
} // namespace api

int main() {
    try {
        using namespace org::openapitools::client::api;
        testExplicitEncoding();
        testDefaultContentTypes();
        testContentDispositionAndMixedEncoding();
        testHeaderInjectionRejected();
        testBoundarySelectionAndWireStructure();
        return EXIT_SUCCESS;
    } catch (const std::exception& exception) {
        std::cerr << "multipart wire test failed: " << exception.what() << '\n';
        return EXIT_FAILURE;
    }
}
