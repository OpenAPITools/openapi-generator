#include <boost/json/src.hpp>

#include "model/RepresentationChecks.h"

#include <cstdlib>
#include <exception>
#include <iostream>
#include <string>
#include <string_view>

namespace model = org::openapitools::client::model;

namespace {

void expectDecodeFailure(
    const std::string& payload,
    const char* expectedDiagnostic,
    const char* message) {
    try {
        model::RepresentationChecks value;
        value.fromJsonString(payload);
    } catch (const std::exception& exception) {
        if (std::string_view(exception.what()).find(expectedDiagnostic) != std::string_view::npos) {
            return;
        }
        std::cerr << message << ": unexpected diagnostic: " << exception.what() << '\n';
        std::exit(1);
    }
    std::cerr << message << ": decode unexpectedly succeeded\n";
    std::exit(1);
}

} // namespace

int main() {
    expectDecodeFailure(
        "{}",
        "Required field 'count' not found",
        "required-property checks must remain active without schema validation");
    expectDecodeFailure(
        R"({"count":2147483648})",
        "not exact",
        "integer range checks must remain active without schema validation");
    expectDecodeFailure(
        R"({"count":1,"ratio":3.5e38})",
        "non-finite destination",
        "non-finite destination checks must remain active without schema validation");

    model::RepresentationChecks valid;
    valid.fromJsonString(R"({"count":7,"ratio":1.25})");
    if (valid.getCount() != 7 || valid.getRatio() != 1.25F) {
        std::cerr << "valid model conversion changed when schema validation was disabled\n";
        return 1;
    }

    std::cout << "validation-disabled runtime checks passed\n";
    return 0;
}
