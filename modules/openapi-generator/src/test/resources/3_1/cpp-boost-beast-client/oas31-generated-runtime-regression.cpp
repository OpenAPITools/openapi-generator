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
#include "model/NullDriftEvent.h"
#include "model/NullDriftResponse.h"
#include "model/NullableEnumBox.h"
#include "model/ScalarDefaults.h"
#include "model/ComposedDefaultContainer.h"
#include "model/StreamChunk.h"
#include "model/TaggedUnionContainer.h"
#include "model/Toggle.h"
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
constexpr char kErrorResponseBody[] =
    R"({"error":{"message":"invalid model ID"}})";


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
        if (target == "/error") {
            return {boost::beast::http::status::bad_request, kErrorResponseBody};
        }
        if (target == "/empty") {
            return {boost::beast::http::status::ok, std::string()};
        }
        return {boost::beast::http::status::bad_request, R"({"kind":"b"})"};
    }

    api::HttpResponseData executeStream(
        const std::string&,
        const std::string& target,
        const std::string&,
        const std::map<std::string, std::string>&,
        api::SseEventCallback onEvent,
        const api::SseStreamOptions& options = {}) override {
        if (target == "/stream-error") {
            return {
                boost::beast::http::status::bad_request,
                {{"Content-Type", "application/json"}},
                kErrorResponseBody};
        }
        if (target == "/stream-wrong-content-type") {
            return {
                boost::beast::http::status::ok,
                {{"Content-Type", "application/json"}},
                R"({"kind":"b"})"};
        }
        if (target != "/stream") {
            return {boost::beast::http::status::bad_request, {}};
        }
        const std::vector<api::SseEvent> events{
            {"delta", R"({"id":"nulls","inline_nullable":null,"referenced_nullable":null,"required_nullable":null,"required_referenced_nullable":null})", "event-1", 250U},
            {"message", R"({"id":"values","inline_nullable":{"value":"inline"},"referenced_nullable":{"value":"reference"},"required_nullable":"required","required_referenced_nullable":{"value":"required reference"}})", "event-2", std::nullopt},
            {"message", R"({"id":"missing","required_nullable":null,"required_referenced_nullable":null})", "event-3", std::nullopt},
            {"message", "[DONE]", "event-4", std::nullopt}};
        for (const api::SseEvent& event : events) {
            if ((options.isCancelled && options.isCancelled()) || !onEvent(event)) {
                return {boost::beast::http::status::ok,
                        {{"Content-Type", "text/event-stream"}}, {}, true, true};
            }
        }
        return {boost::beast::http::status::ok,
                {{"Content-Type", "text/event-stream"}}, {}, true, false};
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

    model::ScalarDefaults defaults;
    expect(!defaults.isEnabled() && defaults.getRetries() == -7,
           "boolean or int32 schema default was not applied");
    expect(defaults.getSequence() == std::numeric_limits<std::int64_t>::min(),
           "int64 minimum schema default was not applied");
    expect(defaults.getRatio() == 0.125 && defaults.getThreshold() == 1.5f,
           "floating-point schema default was not applied");
    expect(defaults.getLabel() == "quoted \"line\"\nnext"
               && defaults.getMode() == "beta",
           "string or enum schema default was not escaped and applied");
    expect(defaults.getNullableLabel().isMissing()
               && defaults.getNullableLabel().isNull(),
           "null schema default did not preserve missing wire presence");
    expect(defaults.getNullableFallback().isMissing()
               && defaults.getNullableFallback().hasValue()
               && defaults.getNullableFallback().value() == "fallback",
           "nullable value default did not preserve missing wire presence");
    expect(defaults.toJsonValue().as_object().empty(),
           "unset schema defaults were serialized onto the wire");

    model::ComposedDefaultContainer composedDefault;
    expect(model::toJsonValue_DefaultVoice(composedDefault.getVoice()).as_string()
               == "alloy",
           "composed schema default did not initialize the variant branch");
    expect(composedDefault.toJsonValue().as_object().empty(),
           "composed schema default was serialized without wire presence");
    composedDefault.fromJsonString("{}");
    expect(model::toJsonValue_DefaultVoice(composedDefault.getVoice()).as_string()
               == "alloy",
           "missing composed property did not restore its schema default");

    defaults.setEnabled(true);
    defaults.setNullableLabel(
        model::NullableField<std::string>::makeNull());
    const boost::json::object explicitDefaults = defaults.toJsonValue().as_object();
    expect(explicitDefaults.at("enabled").as_bool()
               && explicitDefaults.at("nullable_label").is_null(),
           "explicit values did not become wire-present");
    defaults.fromJsonString("{}");
    expect(!defaults.isEnabled() && defaults.getRetries() == -7
               && defaults.getNullableLabel().isMissing()
               && defaults.getNullableLabel().isNull()
               && defaults.toJsonValue().as_object().empty(),
           "repeated deserialization did not restore default value and absence");

    expect(api::toFormParameterValue(direct) == "tag",
           "tagged composition form value exposed its storage wrapper");
    const auto sharedText = std::make_shared<std::string>("json");
    expect(api::toFormParameterValue(sharedText) == "json",
           "shared primitive form value serialized its pointer address");

    std::vector<api::FormParameter> stringVariantParameters;
    api::addVariantFormParameter(stringVariantParameters, "model", direct);
    const std::string stringVariantWire = api::serializeMultipartFormData(
        stringVariantParameters, "VARIANT-BOUNDARY");
    expect(stringVariantWire.find(
               "name=\"model\"\r\nContent-Type: text/plain\r\n\r\ntag\r\n")
               != std::string::npos,
           "tagged string variant did not use the text/plain multipart encoding");
    expect(stringVariantWire.find("\"tag\"") == std::string::npos,
           "tagged string variant was JSON-quoted on the multipart wire");

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

    std::vector<api::FormParameter> objectVariantParameters;
    api::addVariantFormParameter(
        objectVariantParameters, "payload", objectFormChoice.value());
    const std::string objectVariantWire = api::serializeMultipartFormData(
        objectVariantParameters, "VARIANT-BOUNDARY");
    expect(objectVariantWire.find(
               "name=\"payload\"\r\nContent-Type: application/json\r\n\r\n"
               "{\"value\":\"choice\"}\r\n")
               != std::string::npos,
           "object variant did not retain its application/json multipart encoding");

    using ArrayFormChoice = std::variant<std::vector<std::string>, model::Simple>;
    const ArrayFormChoice arrayFormChoice =
        std::vector<std::string>{"alpha", "beta"};
    std::vector<api::FormParameter> arrayVariantParameters;
    api::addVariantFormParameter(
        arrayVariantParameters, "labels", arrayFormChoice);
    const std::string arrayVariantWire = api::serializeMultipartFormData(
        arrayVariantParameters, "VARIANT-BOUNDARY");
    expect(arrayVariantWire.find(
               "name=\"labels\"\r\nContent-Type: text/plain\r\n\r\n"
               "alpha,beta\r\n")
               != std::string::npos,
           "primitive-array variant did not use text/plain form serialization");

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

    model::NullableEnumBox nullableEnums;
    expect(nullableEnums.getOptionalEnum().isMissing(),
           "optional nullable enum did not default to missing");
    nullableEnums.setOptionalEnum(model::NullableField<std::string>::makeNull());
    expect(nullableEnums.getOptionalEnum().isNull(),
           "optional nullable enum rejected null");
    nullableEnums.setOptionalEnum(model::NullableField<std::string>("standard"));
    expect(nullableEnums.getOptionalEnum().value() == "standard",
           "optional nullable enum rejected an allowed value");
    bool rejectedOptionalEnum = false;
    try {
        nullableEnums.setOptionalEnum(model::NullableField<std::string>("invalid"));
    } catch (const std::runtime_error&) {
        rejectedOptionalEnum = true;
    }
    expect(rejectedOptionalEnum,
           "optional nullable enum accepted a disallowed value");

    nullableEnums.setRequiredEnum(std::nullopt);
    expect(!nullableEnums.getRequiredEnum().has_value(),
           "required nullable enum rejected null");
    nullableEnums.setRequiredEnum(std::optional<std::string>("auto"));
    expect(nullableEnums.getRequiredEnum().value() == "auto",
           "required nullable enum rejected an allowed value");
    bool rejectedRequiredEnum = false;
    try {
        nullableEnums.setRequiredEnum(std::optional<std::string>("invalid"));
    } catch (const std::runtime_error&) {
        rejectedRequiredEnum = true;
    }
    expect(rejectedRequiredEnum,
           "required nullable enum accepted a disallowed value");

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

    model::Toggle disabled;
    disabled.fromJsonValue(boost::json::parse(R"({"enabled":false})"));
    expect(!disabled.isEnabled(),
           "false JSON boolean did not overwrite the model property");
    const boost::json::value disabledJson = disabled.toJsonValue();
    expect(disabledJson.as_object().at("enabled").is_bool()
               && !disabledJson.as_object().at("enabled").as_bool(),
           "false model property did not round-trip as false");

    const boost::json::value nullDriftResponseJson = boost::json::parse(
        R"({"id":"response-id","user":null,"error":null,"status_details":null})");
    model::NullDriftResponse nullDriftResponse;
    nullDriftResponse.fromJsonValue(nullDriftResponseJson);
    const boost::json::value nullDriftResponseRoundTrip =
        nullDriftResponse.toJsonValue();
    const boost::json::object& nullDriftResponseObject =
        nullDriftResponseRoundTrip.as_object();
    expect(nullDriftResponseObject.at("id") == "response-id",
           "non-null response field changed while tolerating null drift");
    expect(nullDriftResponseObject.find("user") == nullDriftResponseObject.end()
               && nullDriftResponseObject.find("error") == nullDriftResponseObject.end()
               && nullDriftResponseObject.find("status_details")
                      == nullDriftResponseObject.end(),
           "non-nullable null response fields were not treated as absent");

    expect(!validatesSchema(
               "NullDriftResponse_component",
               R"({"id":"response-id","user":null,"error":null,"status_details":null})"),
           "standalone schema validation became permissive for non-nullable nulls");

    const model::NullDriftEvent nullDriftEvent =
        model::fromJsonValue_NullDriftEvent(boost::json::parse(
            R"({"type":"response.completed","response":{"id":"response-id","user":null,"error":null,"status_details":null}})"));
    const boost::json::value nullDriftEventRoundTrip =
        model::toJsonValue_NullDriftEvent(nullDriftEvent);
    const boost::json::object& nestedResponse =
        nullDriftEventRoundTrip.as_object().at("response").as_object();
    expect(nestedResponse.find("user") == nestedResponse.end()
               && nestedResponse.find("error") == nestedResponse.end()
               && nestedResponse.find("status_details") == nestedResponse.end(),
           "composition branch decoding rejected or retained nested null drift");

    expect(validatesSchema("UnevaluatedAllOf_branch_0", R"({"extra":"ok"})"),
           "unevaluatedProperties allOf schema rejected a matching value");
    expect(!validatesSchema("UnevaluatedAllOf_branch_0", R"({"extra":"x"})"),
           "unevaluatedProperties allOf schema accepted a failing value");
    expect(!validatesSchema(
               "UnevaluatedItemsOnly_branch_0", R"({"extra":[1]})"),
           "nested unevaluatedItems-only schema was treated as an empty schema");
    expect(validatesSchema("ValidationVocabularyInertUnique_branch_0", "[1,1]"),
           "uniqueItems remained active when the validation vocabulary was inert");
    expect(validatesSchema("RefSiblingComposition_component", R"({"target":"ok"})"),
           "$ref with an anyOf sibling rejected an instance matching both schemas");
    expect(!validatesSchema("RefSiblingComposition_component", "{}"),
           "$ref target constraints were skipped beside an anyOf sibling");
    expect(!validatesSchema(
               "RefAnnotationIsolation_component",
               R"({"from_ref":"seen","from_branch":"seen"})"),
           "an anyOf member observed evaluated-property annotations from an adjacent $ref");
    expect(validatesSchema(
               "RefAnnotationIsolation_component", R"({"from_branch":"seen"})"),
           "$ref annotation isolation rejected a branch-local property");
    expect(!validatesSchema(
               "ConditionalAnnotationIsolation_component",
               R"({"from_parent":"seen","from_then":"seen"})"),
           "a then subschema observed annotations from an adjacent parent property");
    expect(validatesSchema(
               "ConditionalAnnotationIsolation_component", R"({"from_then":"seen"})"),
           "conditional annotation isolation rejected a branch-local property");
    expect(!validatesSchema(
               "DependentAnnotationIsolation_component",
               R"({"trigger":true,"from_dependency":"seen"})"),
           "a dependentSchemas subschema observed annotations from an adjacent parent property");
    expect(validatesSchema(
               "DependentAnnotationIsolation_component",
               R"({"from_dependency":"seen"})"),
           "an inactive dependentSchemas entry constrained the instance");
    expect(validatesSchema("MixedPrimitiveEnum_branch_0", "true")
               && validatesSchema("MixedPrimitiveEnum_branch_0", R"("allowed")")
               && validatesSchema("MixedPrimitiveEnum_branch_0", "null"),
           "mixed-kind enum rejected one of its declared values");
    expect(!validatesSchema("MixedPrimitiveEnum_branch_0", "false")
               && !validatesSchema("MixedPrimitiveEnum_branch_0", R"("null")"),
           "mixed-kind enum accepted an undeclared boolean or string");

    api::DefaultApi defaultApi(std::make_shared<FakeHttpClient>());
    std::vector<model::StreamChunk> streamChunks;
    std::vector<api::SseEvent> streamEvents;
    const api::HttpResponseData streamResponse = defaultApi.getStream(
        [&streamChunks, &streamEvents](const model::StreamChunk& chunk,
                                      const api::SseEvent& event) {
            streamChunks.push_back(chunk);
            streamEvents.push_back(event);
            return true;
        });
    expect(streamResponse.status == boost::beast::http::status::ok
               && streamResponse.isEventStream && streamResponse.streamCancelled,
           "the [DONE] terminator did not cancel the event stream");
    expect(streamChunks.size() == 3u,
           "JSON event streaming did not discard the [DONE] terminator");
    expect(streamEvents[0].event == "delta" && streamEvents[0].id == "event-1"
               && streamEvents[0].retryMilliseconds == 250U,
           "typed stream callback discarded SSE wire metadata");
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
    std::size_t cancelledEventCount = 0;
    const api::HttpResponseData cancelledStream = defaultApi.getStream(
        [&cancelledEventCount](const model::StreamChunk&,
                               const api::SseEvent&) {
            ++cancelledEventCount;
            return false;
        });
    expect(cancelledEventCount == 1u && cancelledStream.streamCancelled,
           "stream callback cancellation did not stop delivery");
    bool retainedErrorBody = false;
    try {
        defaultApi.getError();
    } catch (const api::DefaultApiException& exception) {
        retainedErrorBody =
            exception.getStatus() == boost::beast::http::status::bad_request
            && exception.getResponseBody() == kErrorResponseBody;
    }
    expect(retainedErrorBody,
           "non-stream API exception discarded the HTTP response body");

    bool rejectedWrongStreamContentType = false;
    try {
        (void)defaultApi.getWrongContentTypeStream(
            [](const model::StreamChunk&, const api::SseEvent&) { return true; });
    } catch (const api::DefaultApiException& exception) {
        rejectedWrongStreamContentType =
            exception.getStatus() == boost::beast::http::status::ok
            && exception.getResponseBody() == R"({"kind":"b"})";
    }
    expect(rejectedWrongStreamContentType,
           "successful non-SSE response was silently accepted as an event stream");

    bool retainedStreamErrorBody = false;
    try {
        (void)defaultApi.getStreamError(
            [](const model::StreamChunk&, const api::SseEvent&) { return true; });
    } catch (const api::DefaultApiException& exception) {
        retainedStreamErrorBody =
            exception.getStatus() == boost::beast::http::status::bad_request
            && exception.getResponseBody() == kErrorResponseBody;
    }
    expect(retainedStreamErrorBody,
           "stream API exception discarded the HTTP response body");

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
