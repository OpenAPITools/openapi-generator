#include <boost/json/src.hpp>

#include <cstdlib>
#include <iostream>
#include <map>
#include <string>
#include <utility>
#include <variant>

#include "model/ExtraFields.h"
#include "model/ExtraFieldsVariant.h"
#include "model/ExtraFieldsVariant_oneOf.h"

namespace model = org::openapitools::client::model;

namespace {

void expect(bool condition, const char* message) {
    if (!condition) {
        std::cerr << message << '\n';
        std::exit(1);
    }
}

} // namespace

int main() {
    const boost::json::value source = boost::json::parse(
            R"({"name":"known","count":7,"extraJsonProperties":"declared","future":{"nested":[1,true]},"flag":false})");
    model::ExtraFields extraFields;
    extraFields.fromJsonValue(source);

    const model::ExtraFields::ExtraJsonProperties& extras =
            extraFields.getExtraJsonProperties2();
    expect(extras.size() == 2, "model decoding did not retain every unknown field");
    expect(extras.at("future") == boost::json::parse(R"({"nested":[1,true]})"),
            "model decoding changed an unknown object field");
    expect(extras.at("flag").is_bool() && !extras.at("flag").as_bool(),
            "model decoding changed an unknown scalar field");
    expect(extraFields.getExtraJsonProperties() == "declared",
            "declared property sharing the preservation API name did not decode");
    expect(extraFields.toJsonValue() == source,
            "model unknown fields did not survive a decode/encode round trip");

    model::ExtraFields::ExtraJsonProperties conflictingExtras;
    conflictingExtras.emplace("name", boost::json::value("stale"));
    conflictingExtras.emplace("future", boost::json::value("retained"));
    extraFields.setExtraJsonProperties2(std::move(conflictingExtras));
    extraFields.setName("typed");
    const boost::json::object& conflictOutput = extraFields.toJsonValue().as_object();
    expect(conflictOutput.at("name") == "typed",
            "an extra field overrode a typed model property");
    expect(conflictOutput.at("future") == "retained",
            "a non-conflicting extra field was not serialized");

    extraFields.fromJsonValue(boost::json::parse(R"({"name":"fresh"})"));
    expect(extraFields.getExtraJsonProperties2().empty(),
            "repeated decoding retained stale unknown fields");

    const boost::json::value variantSource = boost::json::parse(
            R"({"kind":"first","first":"value","future":{"nested":true}})");
    const model::ExtraFieldsVariant variant =
            model::fromJsonValue_ExtraFieldsVariant(variantSource);
    expect(variant.index() == 0,
            "oneOf decoding did not select the branch with preserved extra fields");
    const auto& firstBranch = std::get<model::ExtraFieldsVariant_oneOf>(variant);
    expect(firstBranch.getExtraJsonProperties().at("future")
                    == boost::json::parse(R"({"nested":true})"),
            "oneOf branch decoding did not retain its unknown field");
    expect(model::toJsonValue_ExtraFieldsVariant(variant) == variantSource,
            "oneOf branch unknown fields did not survive a decode/encode round trip");

    std::cout << "preserve additional properties runtime regressions passed\n";
    return 0;
}
