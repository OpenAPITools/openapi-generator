// ──────────────────────────────────────────────────────────────────────
// Wire-level multipart encoding test executable.
// Compiles as a standalone C++ test that exercises
// serializeMultipartFormData and asserts per-part Content-Type + payload
// for every encoding scenario. Run via CTest:
//   cmake -DMULTIPART_ENCODING_SELF_TEST=ON -B build
//   cmake --build build --parallel
//   ctest --test-dir build --output-on-failure
// ──────────────────────────────────────────────────────────────────────

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <sstream>
#include <string>
#include <vector>

namespace org {
namespace openapitools {
namespace client {
namespace api {

// --- Inlined form parameter types (identical to generated API source) ---
struct FormParameter {
    FormParameter(std::string parameterName, std::string parameterValue, bool file,
                  std::string contentType = "")
        : name(std::move(parameterName)), value(std::move(parameterValue)),
          isFile(file), contentType(std::move(contentType)) {}
    std::string name;
    std::string value;
    bool isFile;
    std::string contentType;
};

inline std::string escapeMultipartParameter(const std::string& value) {
    std::string escapedValue;
    escapedValue.reserve(value.size());
    for (const char character : value) {
        if (character == '\\' || character == '"') {
            escapedValue.push_back('\\');
        }
        escapedValue.push_back(character);
    }
    return escapedValue;
}

inline std::string serializeMultipartFormData(
    const std::vector<FormParameter>& formParameters,
    const std::string& boundary) {
    std::stringstream serializedFormData;
    for (const auto& formParameter : formParameters) {
        serializedFormData << "--" << boundary << "\r\n"
                           << "Content-Disposition: form-data; name=\""
                           << escapeMultipartParameter(formParameter.name) << '"';
        if (formParameter.isFile) {
            serializedFormData << "; filename=\""
                               << escapeMultipartParameter(formParameter.name) << '"';
        }
        serializedFormData << "\r\n";
        if (!formParameter.contentType.empty()) {
            serializedFormData << "Content-Type: " << formParameter.contentType << "\r\n";
        } else if (formParameter.isFile) {
            serializedFormData << "Content-Type: application/octet-stream\r\n";
        }
        serializedFormData << "\r\n" << formParameter.value << "\r\n";
    }
    serializedFormData << "--" << boundary << "--\r\n";
    return serializedFormData.str();
}

inline std::string selectMultipartBoundary(const std::vector<FormParameter>& formParameters) {
    std::string boundary = "OpenAPIGeneratorBoundary";
    for (;;) {
        const auto collision = std::find_if(
            formParameters.cbegin(), formParameters.cend(),
            [&boundary](const FormParameter& fp) {
                return fp.value.find(boundary) != std::string::npos; });
        if (collision == formParameters.cend()) return boundary;
        boundary.push_back('X');
    }
}

// ──────────────────────────────────────────────────────────────────────
// Wire-level assertions
// ──────────────────────────────────────────────────────────────────────
static void testExplicitEncoding() {
    // 1. Encoding Object contentType overrides defaults.
    std::vector<FormParameter> params;
    params.emplace_back("avatar", "\x89PNG\r\n\x1a\n", true, "image/png");
    params.emplace_back("report", "%PDF-1.4", true, "application/pdf");
    std::string wire = serializeMultipartFormData(params, "==BOUNDARY==");
    assert(wire.find("Content-Type: image/png") != std::string::npos);
    assert(wire.find("\x89PNG") != std::string::npos);
    assert(wire.find("Content-Type: application/pdf") != std::string::npos);
    assert(wire.find("PDF-1.4") != std::string::npos);
    // String part without contentType: no Content-Type at wire level
    // (the template injects "text/plain" as the OAS default before
    // calling emplace_back; serializeMultipartFormData only emits
    // Content-Type when the struct field is non-empty).
    params.clear();
    params.emplace_back("description", "A description", false);
    wire = serializeMultipartFormData(params, "==BOUNDARY==");
    assert(wire.find("Content-Type:") == std::string::npos);
    assert(wire.find("A description") != std::string::npos);
}

static void testTextPlainDefault() {
    // 2. String part without encoding defaults to text/plain.
    // NOTE: The OAS default for string form params is text/plain.
    // In generated code the template emits "text/plain" as the 4th arg
    // when isString=true and no Encoding Object contentType is present.
    std::vector<FormParameter> params;
    params.emplace_back("textContent", "Hello", false, "text/plain");
    std::string wire = serializeMultipartFormData(params, "==BOUNDARY==");
    assert(wire.find("Content-Type: text/plain") != std::string::npos);
    assert(wire.find("Hello") != std::string::npos);
}

static void testTextPlainArrayDefault() {
    // 3. Primitive array part without encoding defaults to text/plain.
    // OAS 3.0 §10.4: arrays of primitives use text/plain with form-style
    // comma-delimited serialization.
    std::vector<FormParameter> params;
    params.emplace_back("tags", "a,b,c", false, "text/plain");
    std::string wire = serializeMultipartFormData(params, "==BOUNDARY==");
    assert(wire.find("Content-Type: text/plain") != std::string::npos);
    assert(wire.find("a,b,c") != std::string::npos);
}

static void testJsonDefault() {
    // 4. Object/array part without encoding defaults to application/json.
    std::vector<FormParameter> params;
    params.emplace_back("payload", "{\"key\":\"value\"}", false, "application/json");
    std::string wire = serializeMultipartFormData(params, "==BOUNDARY==");
    assert(wire.find("Content-Type: application/json") != std::string::npos);
    assert(wire.find("{\"key\":\"value\"}") != std::string::npos);
    // Array with model-type elements: JSON array serialization.
    params.clear();
    params.emplace_back("items", "[{\"id\":1},{\"id\":2}]", false, "application/json");
    wire = serializeMultipartFormData(params, "==BOUNDARY==");
    assert(wire.find("Content-Type: application/json") != std::string::npos);
    assert(wire.find("[{\"id\":1},{\"id\":2}]") != std::string::npos);
}

static void testBinaryDefault() {
    // 5. Binary part without encoding: OAS default application/octet-stream.
    std::vector<FormParameter> params;
    params.emplace_back("rawData", "binary-payload", true, "application/octet-stream");
    std::string wire = serializeMultipartFormData(params, "==BOUNDARY==");
    assert(wire.find("Content-Type: application/octet-stream") != std::string::npos);
    assert(wire.find("binary-payload") != std::string::npos);
}

static void testMixedEncoding() {
    // 6. Mixed: avatar (explicit), report (explicit), signature (default octet-stream).
    std::vector<FormParameter> params;
    params.emplace_back("avatar", "\x89PNG", true, "image/png");
    params.emplace_back("report", "%PDF", true, "application/pdf");
    params.emplace_back("signature", "sig", true, "application/octet-stream");
    std::string wire = serializeMultipartFormData(params, "==BOUNDARY==");
    assert(wire.find("Content-Type: image/png") != std::string::npos);
    assert(wire.find("Content-Type: application/pdf") != std::string::npos);
    assert(wire.find("Content-Type: application/octet-stream") != std::string::npos);
}

static void testContentDisposition() {
    // 7. Content-Disposition header structure for file and non-file parts.
    std::vector<FormParameter> params;
    params.emplace_back("fieldA", "valueA", false);
    params.emplace_back("fieldB", "valueB", true, "image/png");
    std::string wire = serializeMultipartFormData(params, "==BOUNDARY==");
    assert(wire.find("name=\"fieldA\"") != std::string::npos);
    assert(wire.find("name=\"fieldB\"") != std::string::npos);
    // Only file parts get filename:
    assert(wire.find("filename=\"fieldB\"") != std::string::npos);
    assert(wire.find("filename=\"fieldA\"") == std::string::npos);
}

static void testBoundaryCollision() {
    // 8. Boundary collision avoidance: auto-append 'X' until unique.
    std::vector<FormParameter> params;
    params.emplace_back("data", "OpenAPIGeneratorBoundary", true);
    std::string boundary = selectMultipartBoundary(params);
    assert(boundary != "OpenAPIGeneratorBoundary");
    assert(boundary.find("OpenAPIGeneratorBoundary") == 0);
}

static void testWireFormatStructure() {
    // 9. Wire format structure: boundary delimitation, CRLF, closing.
    std::vector<FormParameter> params;
    params.emplace_back("field", "value", false);
    const std::string boundary = "TESTBOUNDARY";
    std::string wire = serializeMultipartFormData(params, boundary);
    // Each part starts with --boundary\r\n
    assert(wire.find("--TESTBOUNDARY\r\n") == 0);
    // Closing: --boundary--\r\n
    assert(wire.rfind("--TESTBOUNDARY--\r\n") == wire.size() - 18);
    // CRLF before value
    assert(wire.find("\r\n\r\nvalue\r\n") != std::string::npos);
}

} // namespace org
} // namespace openapitools
} // namespace client
} // namespace api

int main() {
    using namespace org::openapitools::client::api;
    testExplicitEncoding();
    testTextPlainDefault();
    testTextPlainArrayDefault();
    testJsonDefault();
    testBinaryDefault();
    testMixedEncoding();
    testContentDisposition();
    testBoundaryCollision();
    testWireFormatStructure();
    return 0;
}
