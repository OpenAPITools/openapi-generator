#include <boost/json/src.hpp>

#include <cstdlib>
#include <functional>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "model/Mixed.h"
#include "model/AllNull.h"
#include "model/DuplicateNull.h"
#include "model/OuterUnion.h"
#include "model/Simple.h"
#include "model/StreamChunk.h"
#include "model/TaggedUnionContainer.h"
#include "api/DefaultApi.cpp"
#include "model/Oas31ExactJson.h"
#include "model/Oas31Validator.h"
#include "model/Oas31SchemaRegistry.h"

namespace api = org::openapitools::client::api;
namespace model = org::openapitools::client::model;
namespace schema_validation = model::detail::schema_validation;

namespace {

void expect(bool condition, const char* message) {
    if (!condition) {
        std::cerr << message << '\n';
        std::exit(1);
    }
}

bool validatesSchema(std::string const& schemaId, std::string const& payload) {
    schema_validation::ExactJsonValue document = schema_validation::parseExactJson(payload);
    schema_validation::RawInstance instance(&document.value);
    schema_validation::ValidationPath path;
    schema_validation::ValidationContext context;
    schema_validation::SchemaIndex const node = schema_validation::schemaNodeFor(schemaId);
    return node != schema_validation::kNoSchema
        && schema_validation::sharedSchemaEvaluator().validate(node, instance, path, context).success;
}

class FakeHttpClient final : public api::HttpClient {
public:
    std::pair<boost::beast::http::status, std::string> execute(
        const std::string&,
        const std::string& target,
        const std::string&,
        const std::map<std::string, std::string>&) override {
        if (target == "/empty") {
            return {boost::beast::http::status::ok, std::string()};
        }
        return {boost::beast::http::status::bad_request, R"({"kind":"b"})"};
    }

    boost::beast::http::status executeStream(
        const std::string&,
        const std::string& target,
        const std::string&,
        const std::map<std::string, std::string>&,
        std::function<void(const std::string&)> onEvent) override {
        if (target != "/stream") {
            return boost::beast::http::status::bad_request;
        }
        onEvent(R"({"id":"nulls","inline_nullable":null,"referenced_nullable":null,"required_nullable":null,"required_referenced_nullable":null})");
        onEvent(R"({"id":"values","inline_nullable":{"value":"inline"},"referenced_nullable":{"value":"reference"},"required_nullable":"required","required_referenced_nullable":{"value":"required reference"}})");
        onEvent(R"({"id":"missing","required_nullable":null,"required_referenced_nullable":null})");
        onEvent("[DONE]");
        return boost::beast::http::status::ok;
    }
};

} // namespace

int main() {
    bool rejectedMissingBase = false;
    try {
        (void)model::fromJsonValue_Mixed(
            boost::json::parse(R"({"kind":"b"})"));
    } catch (const std::invalid_argument&) {
        rejectedMissingBase = true;
    }
    expect(rejectedMissingBase,
           "mixed allOf/oneOf schema accepted an instance missing allOf requirements");

    const model::Mixed mixed = model::fromJsonValue_Mixed(
        boost::json::parse(R"({"base":"present","kind":"b"})"));
    expect(mixed.index() == 1,
           "composition conversion ignored the validator-selected branch");
    auto expectOuterUnionRoundTrip = [](const char* payload, const char* message) {
        const boost::json::value source = boost::json::parse(payload);
        const model::OuterUnion converted = model::fromJsonValue_OuterUnion(source);
        expect(model::toJsonValue_OuterUnion(converted) == source, message);
    };
    expectOuterUnionRoundTrip(R"("tag")",
           "nested short-string union branch failed to round-trip");
    expectOuterUnionRoundTrip(R"("nested")",
           "nested long-string union branch failed to round-trip");
    expectOuterUnionRoundTrip("true",
           "outer boolean union branch failed to round-trip");

    const model::NestedUnion direct = model::makeNestedUnionBranch0("tag");
    const model::NestedUnion nested = model::makeNestedUnionBranch1("nested");
    model::TaggedUnionContainer taggedContainer;
    taggedContainer.setDirect(direct);
    taggedContainer.setNested(model::OuterUnion(nested));
    const boost::json::value taggedContainerJson =
        boost::json::parse(R"({"direct":"tag","nested":"nested"})");
    expect(taggedContainer.toJsonValue() == taggedContainerJson,
           "tagged variant properties failed to encode");

    model::TaggedUnionContainer decodedTaggedContainer;
    decodedTaggedContainer.fromJsonValue(taggedContainerJson);
    expect(decodedTaggedContainer.toJsonValue() == taggedContainerJson,
           "tagged variant properties failed to round-trip");

    expect(api::toFormParameterValue(direct) == "tag",
           "tagged composition form value exposed its storage wrapper");
    const auto sharedText = std::make_shared<std::string>("json");
    expect(api::toFormParameterValue(sharedText) == "json",
           "shared primitive form value serialized its pointer address");

    using FormChoice = std::variant<std::string, model::Simple>;
    const std::optional<FormChoice> primitiveFormChoice =
        FormChoice(std::string("auto"));
    expect(api::toFormParameterValue(primitiveFormChoice) == "auto",
           "primitive variant form value failed to serialize");
    model::Simple simpleFormChoice;
    simpleFormChoice.setValue("choice");
    const std::optional<FormChoice> objectFormChoice =
        FormChoice(std::move(simpleFormChoice));
    expect(api::toFormParameterValue(objectFormChoice)
               == R"({"value":"choice"})",
           "model variant form value failed to serialize as JSON");

    const std::map<std::string, std::string> formMap{{"key", "value"}};
    expect(api::toFormParameterValue(formMap) == R"({"key":"value"})",
           "map form value failed to serialize as JSON");

    (void)model::fromJsonValue_AllNull(boost::json::value(nullptr));
    bool rejectedDuplicateNull = false;
    try {
        (void)model::fromJsonValue_DuplicateNull(boost::json::value(nullptr));
    } catch (const std::invalid_argument&) {
        rejectedDuplicateNull = true;
    }
    expect(rejectedDuplicateNull,
           "duplicate-null oneOf accepted a value matching both branches");

    model::DuplicateNull taggedNull = model::makeDuplicateNullBranch0(nullptr);
    expect(model::isDuplicateNullBranch0(taggedNull),
           "model-qualified branch predicate did not identify branch zero");
    expect(model::getDuplicateNullBranch0(taggedNull) == nullptr,
           "model-qualified branch getter returned the wrong value");

    bool rejectedUnrepresentableNumber = false;
    try {
        schema_validation::ExactJsonValue exactNumber = schema_validation::parseExactJson("1e9999999999");
        schema_validation::requireModelConvertibleJson(exactNumber);
    } catch (const std::invalid_argument&) {
        rejectedUnrepresentableNumber = true;
    }
    expect(rejectedUnrepresentableNumber,
           "public model conversion accepted an unrepresentable numeric DOM");

    expect(validatesSchema("nonAsciiPattern_branch_0", R"({"ármányos":1})"),
           "patternProperties failed to match a non-ASCII object key");
    expect(!validatesSchema("nonAsciiPattern_branch_0", R"({"élmény":1})"),
           "additionalProperties false accepted an unmatched object key");

    api::DefaultApi defaultApi(std::make_shared<FakeHttpClient>());
    const std::vector<model::StreamChunk> streamChunks = defaultApi.getStream();
    expect(streamChunks.size() == 3u,
           "JSON event streaming did not discard the [DONE] terminator");
    expect(streamChunks[0].getInlineNullable().isNull(),
           "inline nullable object did not decode explicit null");
    expect(streamChunks[0].getReferencedNullable().isNull(),
           "nullable reference did not decode explicit null");
    expect(!streamChunks[0].getRequiredNullable().has_value(),
           "required nullable scalar did not decode explicit null");
    expect(!streamChunks[0].getRequiredReferencedNullable().has_value(),
           "required nullable reference did not decode explicit null");
    expect(streamChunks[1].getInlineNullable().hasValue()
               && streamChunks[1].getInlineNullable().value().getValue() == "inline",
           "inline nullable object did not decode a value");
    expect(streamChunks[1].getReferencedNullable().hasValue()
               && streamChunks[1].getReferencedNullable().value().getValue() == "reference",
           "nullable reference did not decode a value");
    expect(streamChunks[1].getRequiredNullable().value() == "required",
           "required nullable scalar did not decode a value");
    expect(streamChunks[1].getRequiredReferencedNullable().has_value()
               && streamChunks[1].getRequiredReferencedNullable().value().getValue()
                      == "required reference",
           "required nullable reference did not decode a value");
    expect(streamChunks[2].getInlineNullable().isMissing(),
           "missing inline nullable object lost its missing state");
    expect(streamChunks[2].getReferencedNullable().isMissing(),
           "missing nullable reference lost its missing state");
    expect(!streamChunks[2].getRequiredReferencedNullable().has_value(),
           "required nullable reference did not preserve explicit null");
    (void)defaultApi.getEmpty();
    const api::GetProbeResponse response = defaultApi.getProbe();
    const model::Fallback& fallback = std::get<model::Fallback>(response.body);
    expect(fallback.index() == 1,
           "default response bypassed composed-model validation");

    boost::json::object object;
    object["R"] = 100;
    object["G"] = 200;
    const boost::json::value objectValue(std::move(object));

    expect(api::pathStyleValue(objectValue, "matrix", true, "color")
               == ";R=100;G=200",
           "exploded matrix object used the parameter name instead of member names");

    std::stringstream deepObject;
    const char* deepObjectSeparator = "";
    api::appendParamQueryParameter(
        deepObject, deepObjectSeparator, "color", objectValue,
        "deepObject", true, false, false);
    expect(deepObject.str() == "color[R]=100&color[G]=200",
           "deepObject did not use the OpenAPI bracket delimiters");

    const std::optional<std::map<std::string, std::string>> optionalMetadata =
        std::map<std::string, std::string>{{"key", "value"}};
    std::stringstream metadataQuery;
    const char* metadataSeparator = "";
    api::appendParamQueryParameter(
        metadataQuery, metadataSeparator, "metadata", optionalMetadata,
        "form", true, false, false);
    expect(metadataQuery.str() == "key=value",
           "optional object query parameter lost form explode semantics");

    const std::vector<std::shared_ptr<std::string>> pointerValues{
        std::make_shared<std::string>("alpha"),
        std::make_shared<std::string>("beta")};
    std::stringstream pointerQuery;
    const char* pointerSeparator = "";
    api::appendParamQueryParameter(
        pointerQuery, pointerSeparator, "value", pointerValues,
        "form", true, false, false);
    expect(pointerQuery.str() == "value=alpha&value=beta",
           "shared primitive query values failed recursive conversion");

    std::stringstream reserved;
    const char* reservedSeparator = "";
    api::appendParamQueryParameter(
        reserved, reservedSeparator, "value", std::string("a%2Fb%zz c"),
        "form", false, true, false);
    expect(reserved.str() == "value=a%2Fb%25zz%20c",
           "reserved query expansion did not preserve valid percent triples");

    std::string cookie;
    api::appendCookieParameter(
        cookie, "session", std::string("abc; admin=true"), false);
    expect(cookie == "session=abc%3B%20admin%3Dtrue",
           "cookie data was not percent-encoded before header assembly");

    std::cout << "oas31 generated runtime regressions passed\n";
    return 0;
}
